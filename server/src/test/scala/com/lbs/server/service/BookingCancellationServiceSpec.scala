package com.lbs.server.service

import com.lbs.api.http.LuxmedResponse
import com.lbs.api.json.model.Event
import com.lbs.server.ThrowableOr
import com.lbs.server.rest.{ApiResponse, BookingAttemptService, BookingRejectedException, CancellationReviewRequest, LegacyBookingBarrier, LuxmedRestController}
import com.lbs.server.util.DateTimeUtil
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import org.mockito.ArgumentMatchers.{any, anyLong}
import org.mockito.Mockito.{doAnswer, mock, verify, when}
import org.springframework.jdbc.core.JdbcTemplate
import org.springframework.jdbc.datasource.DriverManagerDataSource

import java.time.{LocalDateTime, ZonedDateTime}
import java.util.UUID
import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.AtomicReference

class BookingCancellationServiceSpec {
  private val visit = Event(ZonedDateTime.now(DateTimeUtil.Zone).plusDays(2), None, None, 77L, "Reserved", "Visit")

  private class FakeApi extends ApiService {
    var upcoming: ThrowableOr[List[Event]] = Right(List(visit))
    var verified: List[ThrowableOr[List[Event]]] = List(Right(List(visit)))
    var deletion: ThrowableOr[LuxmedResponse[String]] = Right(LuxmedResponse("", 200, Nil))
    var reads = 0
    var deletes = 0
    var lastCoverage: Option[(ZonedDateTime, ZonedDateTime)] = None

    override def reserved(accountId: Long, fromDate: LocalDateTime, toDate: LocalDateTime): ThrowableOr[List[Event]] = upcoming
    override def reservedVerified(accountId: Long, fromDate: ZonedDateTime, toDate: ZonedDateTime): ThrowableOr[List[Event]] = {
      reads += 1
      lastCoverage = Some((fromDate, toDate))
      val result = verified.headOption.getOrElse(Left(new IllegalStateException("No verified response")))
      verified = verified.drop(1)
      result
    }
    override def deleteReservation(accountId: Long, reservationId: Long): ThrowableOr[LuxmedResponse[String]] = {
      deletes += 1
      deletion
    }
  }

  private def fixture(): (BookingCancellationService, FakeApi, BookingAttemptService, AccountBookingFence, CancellationReceiptService) = {
    val api = new FakeApi()
    val attempts = mock(classOf[BookingAttemptService])
    when(attempts.legacyBarrier(1L)).thenReturn(LegacyBookingBarrier("clear"))
    val fence = new AccountBookingFence()
    val receipts = mock(classOf[CancellationReceiptService])
    (new BookingCancellationService(api, attempts, fence, receipts), api, attempts, fence, receipts)
  }

  @Test def aSuccessfulDeleteStillRequiresOperatorReview(): Unit = {
    val (cancellation, api, attempts, _, _) = fixture()
    assertEquals("CANCELLATION_REVIEW_REQUIRED",
      cancellation.cancel(1L, 77L).left.toOption.get.asInstanceOf[BookingRejectedException].code)
    assertEquals(1, api.reads)
    assertEquals(1, api.deletes)
    verify(attempts).beginCancellation(1L, 77L, visit.date.toInstant.toEpochMilli)
  }

  @Test def pendingSmartOrLegacyOutcomePreventsDeletion(): Unit = {
    val (cancellation, api, attempts, _, _) = fixture()
    when(attempts.accountBusy(1L)).thenReturn(true)
    assertTrue(cancellation.cancel(1L, 77L).left.toOption.get.isInstanceOf[BookingRejectedException])
    assertEquals(0, api.deletes)

    when(attempts.accountBusy(1L)).thenReturn(false)
    when(attempts.legacyBarrier(1L)).thenReturn(LegacyBookingBarrier("pending"))
    assertTrue(cancellation.cancel(1L, 77L).isLeft)
    assertEquals(0, api.deletes)
  }

  @Test def aChangedCancellationTargetCannotBeDeleted(): Unit = {
    val (cancellation, api, _, _, _) = fixture()
    assertTrue(cancellation.cancel(1L, 77L, Some(visit.date.toInstant.toEpochMilli + 60000)).isLeft)
    assertEquals(0, api.deletes)
    assertEquals(0, api.reads)
  }

  @Test def redirectAndBusinessErrorBodiesDoNotConfirmCancellation(): Unit = {
    for ((code, body) <- List((302, ""), (200, "{\"hasErrors\":true}"), (200, "error"))) {
      val (cancellation, api, _, _, _) = fixture()
      api.deletion = Right(LuxmedResponse(body, code, Nil))
      assertTrue(cancellation.cancel(1L, 77L).isLeft)
      assertEquals(1, api.deletes)
      assertEquals(1, api.reads)
    }
  }

  @Test def incompletePreDeleteFeedPreventsDeletion(): Unit = {
    val (beforeCancel, beforeApi, _, _, _) = fixture()
    beforeApi.verified = List(Left(new IllegalStateException("Incomplete feed")))
    assertTrue(beforeCancel.cancel(1L, 77L).isLeft)
    assertEquals(0, beforeApi.deletes)
  }

  @Test def providerErrorLeavesReceiptPendingWithoutAnyFollowupRead(): Unit = {
    val (cancellation, api, attempts, _, _) = fixture()
    api.deletion = Left(new IllegalStateException("Provider response lost"))
    assertTrue(cancellation.cancel(1L, 77L).isLeft)
    assertEquals(1, api.reads)
    verify(attempts).beginCancellation(1L, 77L, visit.date.toInstant.toEpochMilli)
  }

  @Test def cancellationWaitsForTheAccountBookingFence(): Unit = {
    val (cancellation, api, _, fence, _) = fixture()
    val entered = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val lockHolder = new Thread(() => { fence.withLock(1L) { entered.countDown(); release.await(2, TimeUnit.SECONDS) }; () })
    lockHolder.start()
    assertTrue(entered.await(2, TimeUnit.SECONDS))
    val result = new AtomicReference[ThrowableOr[Unit]]()
    val cancellationThread = new Thread(() => result.set(cancellation.cancel(1L, 77L)))
    cancellationThread.start()
    Thread.sleep(100)
    assertTrue(cancellationThread.isAlive)
    assertEquals(0, api.deletes)
    release.countDown()
    cancellationThread.join(2000)
    lockHolder.join(2000)
    assertEquals("CANCELLATION_REVIEW_REQUIRED",
      result.get().left.toOption.get.asInstanceOf[BookingRejectedException].code)
  }

  @Test def restCancellationReturnsConflictForAnUnresolvedBooking(): Unit = {
    val (cancellation, api, attempts, _, _) = fixture()
    when(attempts.accountBusy(1L)).thenReturn(true)
    val controller = new LuxmedRestController()
    val field = classOf[LuxmedRestController].getDeclaredField("bookingCancellation")
    field.setAccessible(true)
    field.set(controller, cancellation)
    assertEquals(409, controller.cancelVisit(1L, 77L).getStatusCode.value())
    assertEquals(0, api.deletes)
  }

  @Test def repeatedRestCancellationReturnsTypedConflictWithoutSecondDelete(): Unit = {
    val api = new FakeApi()
    api.verified = List(Right(List(visit)), Right(List(visit)))
    val attempts = mock(classOf[BookingAttemptService])
    when(attempts.legacyBarrier(1L)).thenReturn(LegacyBookingBarrier("clear"))
    val source = new DriverManagerDataSource(s"jdbc:h2:mem:${UUID.randomUUID()};DB_CLOSE_DELAY=-1", "sa", "")
    val jdbc = new JdbcTemplate(source)
    jdbc.execute("CREATE TABLE cancellation_receipt(account_id BIGINT NOT NULL, reservation_id BIGINT NOT NULL, start_at BIGINT NOT NULL, state VARCHAR(32) NOT NULL, requested_at BIGINT NOT NULL, confirmed_at BIGINT, reviewed_at BIGINT, reviewed_by VARCHAR(128), review_reason VARCHAR(1000), review_action VARCHAR(32), PRIMARY KEY(account_id,reservation_id))")
    jdbc.execute("ALTER TABLE cancellation_receipt ADD COLUMN moved_acknowledged_at BIGINT")
    val receipts = new CancellationReceiptService(jdbc)
    doAnswer(_ => { receipts.begin(1L, 77L, visit.date.toInstant.toEpochMilli); null })
      .when(attempts).beginCancellation(1L, 77L, visit.date.toInstant.toEpochMilli)
    val cancellation = new BookingCancellationService(api, attempts, new AccountBookingFence(), receipts)
    val controller = new LuxmedRestController()
    val field = classOf[LuxmedRestController].getDeclaredField("bookingCancellation")
    field.setAccessible(true)
    field.set(controller, cancellation)

    val first = controller.cancelVisit(1L, 77L)
    assertEquals(409, first.getStatusCode.value())
    assertEquals(Some("CANCELLATION_REVIEW_REQUIRED"), first.getBody.asInstanceOf[ApiResponse[Any]].error)
    val repeated = controller.cancelVisit(1L, 77L)
    assertEquals(409, repeated.getStatusCode.value())
    assertEquals(Some("CANCELLATION_ATTEMPT_ALREADY_EXISTS"), repeated.getBody.asInstanceOf[ApiResponse[Any]].error)
    assertEquals(1, api.deletes)
  }

  @Test def reviewUsesTheAccountFenceAndNeverCallsProviderDelete(): Unit = {
    val (cancellation, api, attempts, fence, _) = fixture()
    api.verified = List(Right(List(visit)), Right(List(visit)))
    val startAt = visit.date.toInstant.toEpochMilli
    when(attempts.reviewCancellation(1L, 77L, startAt, "verified_still_reserved", "operator", "Still on account",
      true, false, None, List(visit)))
      .thenReturn(CancellationReceipt(1L, 77L, startAt, "verified_still_reserved"))
    val entered = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val holder = new Thread(() => { fence.withLock(1L) { entered.countDown(); release.await(2, TimeUnit.SECONDS) }; () })
    holder.start()
    assertTrue(entered.await(2, TimeUnit.SECONDS))
    val result = new AtomicReference[CancellationReceipt]()
    val reviewer = new Thread(() => result.set(cancellation.review(1L, 77L, startAt,
      "verified_still_reserved", "operator", "Still on account", true, false)))
    reviewer.start()
    Thread.sleep(100)
    assertTrue(reviewer.isAlive)
    release.countDown()
    reviewer.join(2000)
    holder.join(2000)
    assertEquals("verified_still_reserved", result.get().state)
    assertEquals(0, api.deletes)

    val controller = new LuxmedRestController()
    val field = classOf[LuxmedRestController].getDeclaredField("bookingCancellation")
    field.setAccessible(true)
    field.set(controller, cancellation)
    assertEquals(200, controller.reviewCancellation(1L, 77L,
      CancellationReviewRequest(startAt, "verified_still_reserved", "operator", "Still on account", true))
      .getStatusCode.value())
  }

  @Test def reviewRequiresSettledProviderAndCompleteExactDayFeed(): Unit = {
    val (cancellation, api, attempts, _, _) = fixture()
    val startAt = visit.date.toInstant.toEpochMilli
    assertThrows(classOf[BookingRejectedException], () => cancellation.review(1L, 77L, startAt,
      "confirmed_cancelled", "operator", "Checked portal", false, false))
    assertEquals(0, api.reads)
    assertThrows(classOf[BookingRejectedException], () => cancellation.review(1L, 77L, startAt,
      "confirmed_cancelled", "operator", "Checked portal", true, false))
    assertEquals(0, api.reads)
    api.verified = List(Left(new IllegalStateException("Incomplete feed")))
    assertThrows(classOf[BookingRejectedException], () => cancellation.review(1L, 77L, startAt,
      "confirmed_cancelled", "operator", "Checked portal", true, true))
    assertTrue(api.lastCoverage.get._2.isAfter(visit.date.plusMonths(11)),
      "cancelled review must check a broad future range for moved reservations")
    verify(attempts, org.mockito.Mockito.never()).reviewCancellation(anyLong(), anyLong(), anyLong(),
      any(), any(), any(), org.mockito.ArgumentMatchers.anyBoolean(),
      org.mockito.ArgumentMatchers.anyBoolean(), any(), any())
  }

  @Test def movedReviewRequiresCompleteCoverageOfOldAndNewDays(): Unit = {
    val (cancellation, api, attempts, _, _) = fixture()
    val original = visit.date.toInstant.toEpochMilli
    val moved = visit.copy(date = visit.date.plusDays(2),
      dateTo = Some(visit.date.plusDays(2).plusMinutes(30)),
      eventType = Some("Telemedicine"))
    val movedAt = moved.date.toInstant.toEpochMilli
    api.verified = List(Right(List(moved)))
    when(attempts.reviewCancellation(1L, 77L, original, "verified_moved", "operator", "Moved in portal",
      true, false, Some(movedAt), List(moved)))
      .thenReturn(CancellationReceipt(1L, 77L, original, "verified_moved", movedStartAt = Some(movedAt)))
    val reviewed = cancellation.review(1L, 77L, original, "verified_moved", "operator", "Moved in portal",
      true, false, Some(movedAt))
    assertEquals("verified_moved", reviewed.state)
    assertFalse(api.lastCoverage.get._1.isAfter(visit.date))
    assertTrue(api.lastCoverage.get._2.isAfter(moved.date))

    api.verified = List(Left(new IllegalStateException("Incomplete coverage")))
    assertThrows(classOf[BookingRejectedException], () => cancellation.review(1L, 77L, original,
      "verified_moved", "operator", "Moved in portal", true, false, Some(movedAt)))
  }
}
