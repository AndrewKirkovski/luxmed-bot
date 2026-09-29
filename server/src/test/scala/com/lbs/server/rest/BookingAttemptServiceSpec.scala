package com.lbs.server.rest

import com.lbs.api.json.model.{AdditionalData, Doctor, LuxmedFunnyDateTime, RelatedVisit, ReservationConfirmResponse, ReservationConfirmValue, ReservationLocktermResponse, ReservationLocktermResponseValue, Term, TermExt, Valuation, XsrfToken}
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions.*
import org.springframework.jdbc.core.JdbcTemplate
import org.springframework.jdbc.datasource.{DataSourceTransactionManager, DriverManagerDataSource}
import java.util.UUID
import java.time.{LocalDateTime, LocalTime, ZoneId, ZonedDateTime}
import com.lbs.server.repository.model.Monitoring
import com.lbs.server.repository.DataRepository
import com.lbs.bot.model.{MessageSource, MessageSourceSystem}
import com.lbs.server.service.{AccountBookingFence, ApiService, CancellationReceiptService, DataService, MonitoringService}
import org.mockito.ArgumentMatchers.{any, anyLong}
import org.mockito.Mockito.{mock, when}

class BookingAttemptServiceSpec {
  private def fixtureWithDb(): (BookingAttemptService, JdbcTemplate) = {
    val source = new DriverManagerDataSource(s"jdbc:h2:mem:${UUID.randomUUID()};DB_CLOSE_DELAY=-1", "sa", "")
    val jdbc = new JdbcTemplate(source)
    jdbc.execute("CREATE TABLE booking_attempt(id VARCHAR(36) PRIMARY KEY, account_id BIGINT NOT NULL, fingerprint VARCHAR(64) NOT NULL, state VARCHAR(20) NOT NULL, reservation_id BIGINT, error_code VARCHAR(64), created_at BIGINT NOT NULL, phase VARCHAR(24), process_id VARCHAR(36))")
    jdbc.execute("CREATE TABLE booking_account_lock(account_id BIGINT PRIMARY KEY, attempt_id VARCHAR(36) NOT NULL)")
    jdbc.execute("CREATE TABLE legacy_booking_barrier(id VARCHAR(36) PRIMARY KEY, account_id BIGINT NOT NULL, state VARCHAR(20) NOT NULL, reservation_id BIGINT, monitoring_id BIGINT, start_at BIGINT NOT NULL, created_at BIGINT NOT NULL, phase VARCHAR(24), process_id VARCHAR(36))")
    jdbc.execute("CREATE TABLE smart_booking_enrollment(account_id BIGINT PRIMARY KEY, identity_key VARCHAR(255) NOT NULL UNIQUE, enrolled_at BIGINT NOT NULL)")
    jdbc.execute("CREATE TABLE monitoring(record_id BIGINT PRIMARY KEY, account_id BIGINT NOT NULL, active BOOLEAN NOT NULL, autobook BOOLEAN NOT NULL)")
    jdbc.execute("CREATE TABLE credentials(account_id BIGINT NOT NULL, username VARCHAR(255) NOT NULL)")
    jdbc.execute("CREATE TABLE cancellation_receipt(account_id BIGINT NOT NULL, reservation_id BIGINT NOT NULL, start_at BIGINT NOT NULL, state VARCHAR(20) NOT NULL, requested_at BIGINT NOT NULL, confirmed_at BIGINT, PRIMARY KEY(account_id,reservation_id))")
    jdbc.update("INSERT INTO credentials(account_id,username) VALUES (?,?)", Long.box(1L), "person@example.com")
    (new BookingAttemptService(jdbc, new DataSourceTransactionManager(source), new CancellationReceiptService(jdbc)), jdbc)
  }
  private def fixture(): BookingAttemptService = fixtureWithDb()._1
  private def request(id: String = UUID.randomUUID().toString): BookRequest =
    BookRequest(1, 2, 2, "Clinic", 3, "First", "Last", "", 4, 5, 6,
      "2026-10-06T12:00:00", "2026-10-06T12:30:00", attemptId = Some(id))
  private def runLegacyMonitorBooking(service: BookingAttemptService, api: ApiService): Unit = {
    val monitoring = new Monitoring()
    monitoring.recordId = 10L
    monitoring.accountId = 1L
    monitoring.cityId = 1L
    monitoring.clinicId = 2L
    monitoring.serviceId = 3L
    monitoring.doctorId = 4L
    monitoring.dateFrom = ZonedDateTime.of(2026, 6, 1, 0, 0, 0, 0, ZoneId.of("Europe/Warsaw"))
    monitoring.dateTo = monitoring.dateFrom.plusDays(1)
    monitoring.timeFrom = LocalTime.of(8, 0)
    monitoring.timeTo = LocalTime.of(18, 0)
    monitoring.active = true
    val date = LocalDateTime.of(2026, 6, 1, 10, 0)
    val funnyDate = LuxmedFunnyDateTime(dateTimeLocal = Some(date))
    val doctor = Doctor(Some("dr"), Some(List(4L)), Some("John"), Some(false), Some(1L), 4L, Some("Smith"))
    val term = TermExt(AdditionalData(false, Nil),
      Term(Some("Clinic"), 2L, 3L, funnyDate, funnyDate, doctor, Some(""),
        false, false, false, 1L, 1000L, 100L))
    val data = mock(classOf[DataService])
    when(data.findMonitoring(anyLong(), anyLong())).thenReturn(Some(monitoring))
    when(api.getAvailableTerms(anyLong(), anyLong(), any(), anyLong(), any(), any(), any(), any(), any(), anyLong()))
      .thenReturn(Right(List(term)))
    when(api.getXsrfToken(anyLong())).thenReturn(Right(XsrfToken("tok", Seq.empty)))
    val monitorings = new MonitoringService()
    for ((name, value) <- List(
      "dataService" -> data,
      "apiService" -> api,
      "bookingFence" -> new AccountBookingFence(),
      "bookingAttempts" -> service
    )) {
      val field = classOf[MonitoringService].getDeclaredField(name)
      field.setAccessible(true)
      field.set(monitorings, value)
    }
    val time = java.time.Duration.between(LocalDateTime.of(2022, 1, 1, 0, 0), date).toMinutes
    monitorings.bookAppointmentByScheduleId(1L, 10L, 1000L, time)
  }
  private val success = ReservationConfirmResponse(Nil, Nil, false, false, ReservationConfirmValue(false, "fixture", 77, 88))
  private def controllerWith(service: BookingAttemptService, monitorings: MonitoringService,
                             api: ApiService = null, smartEnrolled: Boolean = true): LuxmedRestController = {
    if (smartEnrolled) service.enrollSmartBooking(1L)
    val controller = new LuxmedRestController()
    for ((name, value) <- List(
      "monitoringService" -> monitorings,
      "bookingAttempts" -> service,
      "bookingFence" -> new AccountBookingFence(),
      "apiService" -> api
    )) {
      val field = classOf[LuxmedRestController].getDeclaredField(name)
      field.setAccessible(true)
      field.set(controller, value)
    }
    controller
  }

  @Test def replayReturnsStoredReceiptWithoutAnotherBooking(): Unit = {
    val service = fixture()
    val req = request()
    var calls = 0
    def book() = { calls += 1; Right(success) }
    assertEquals("succeeded", service.execute(1, req)(() => book()).state)
    assertEquals(Some(77L), service.execute(1, req)(() => book()).reservationId)
    assertEquals(1, calls)
    assertTrue(service.status(2, req.attemptId.get).isEmpty)
  }

  @Test def uncertainOutcomeHoldsAccountAndCannotBeRetried(): Unit = {
    val service = fixture()
    val req = request()
    assertEquals("unknown", service.execute(1, req)(() => Left(new java.net.SocketTimeoutException())).state)
    assertEquals("unknown", service.execute(1, req)(() => throw new AssertionError("must not retry")).state)
    val denied = request()
    assertEquals("failed", service.execute(1, denied)(() => throw new AssertionError("account must be held")).state)
    assertEquals(Some("ACCOUNT_BUSY"), service.status(1, denied.attemptId.get).get.errorCode)
    assertTrue(service.accountBusy(1))
  }

  @Test def restartReleasesOnlyPreparedSmartAndLegacyWork(): Unit = {
    val (service, jdbc) = fixtureWithDb()
    val preparedId = UUID.randomUUID().toString
    val confirmedId = UUID.randomUUID().toString
    val oldId = UUID.randomUUID().toString
    jdbc.update("INSERT INTO booking_attempt(id,account_id,fingerprint,state,created_at,phase,process_id) VALUES (?,?,?,'pending',1,'prepared','prior-boot')",
      preparedId, Long.box(1L), "fixture")
    jdbc.update("INSERT INTO booking_account_lock(account_id,attempt_id) VALUES (?,?)", Long.box(1L), preparedId)
    jdbc.update("INSERT INTO booking_attempt(id,account_id,fingerprint,state,created_at,phase,process_id) VALUES (?,?,?,'pending',1,'confirmation_started','prior-boot')",
      confirmedId, Long.box(2L), "fixture")
    jdbc.update("INSERT INTO booking_account_lock(account_id,attempt_id) VALUES (?,?)", Long.box(2L), confirmedId)
    jdbc.update("INSERT INTO booking_attempt(id,account_id,fingerprint,state,created_at) VALUES (?,?,?,'pending',1)",
      oldId, Long.box(3L), "fixture")
    jdbc.update("INSERT INTO booking_account_lock(account_id,attempt_id) VALUES (?,?)", Long.box(3L), oldId)
    val legacyPrepared = service.beginLegacyBooking(4L, 100L)
    val legacyConfirmed = service.beginLegacyBooking(5L, 100L)
    assertTrue(service.markLegacyConfirmationStarted(5L, legacyConfirmed).isRight)
    val restarted = new BookingAttemptService(jdbc, new DataSourceTransactionManager(jdbc.getDataSource),
      new CancellationReceiptService(jdbc))
    assertEquals(2, restarted.recoverPreparedAfterRestart())
    assertEquals("failed", restarted.status(1L, preparedId).get.state)
    assertEquals(Some("PREPARED_INTERRUPTED"), restarted.status(1L, preparedId).get.errorCode)
    assertFalse(restarted.accountBusy(1L))
    assertEquals("pending", restarted.status(2L, confirmedId).get.state)
    assertTrue(restarted.accountBusy(2L))
    assertEquals("pending", restarted.status(3L, oldId).get.state)
    assertTrue(restarted.accountBusy(3L))
    assertEquals("clear", restarted.legacyBarrier(4L).state)
    assertEquals("pending", restarted.legacyBarrier(5L).state)
    assertEquals(0, restarted.recoverPreparedAfterRestart())
    assertFalse(service.markLegacyConfirmationStarted(4L, legacyPrepared).isRight)
  }

  @Test def competingProcessRecoveryPreventsSmartConfirmation(): Unit = {
    val (service, jdbc) = fixtureWithDb()
    val restarted = new BookingAttemptService(jdbc, new DataSourceTransactionManager(jdbc.getDataSource),
      new CancellationReceiptService(jdbc))
    val api = mock(classOf[ApiService])
    val monitorings = mock(classOf[MonitoringService])
    when(monitorings.getActiveMonitorings(1L)).thenReturn(Seq.empty)
    val controller = controllerWith(service, monitorings, api)
    val valuation = Valuation(None, None, false, false, None, Some(0.0), None, None, None, false, 1)
    val lock = ReservationLocktermResponse(Nil, Nil, false, false,
      ReservationLocktermResponseValue(false, None, null, Nil, 123L, List(valuation)))
    when(api.getXsrfToken(anyLong())).thenReturn(Right(XsrfToken("tok", Seq.empty)))
    when(api.reservationLockterm(anyLong(), any(), any())).thenAnswer(_ => {
      assertEquals(1, restarted.recoverPreparedAfterRestart())
      Right(lock)
    })
    val outcome = controller.submitBookingAttempt(1L, request().copy(isImpediment = Some(false)))
      .getBody.asInstanceOf[ApiResponse[BookingOutcome]].data.get
    assertEquals("failed", outcome.state)
    assertFalse(service.accountBusy(1L))
    org.mockito.Mockito.verify(api, org.mockito.Mockito.never()).reservationConfirm(anyLong(), any(), any())
    org.mockito.Mockito.verify(api, org.mockito.Mockito.never()).reservationChangeTerm(anyLong(), any(), any())
  }

  @Test def confirmationMarkerSurvivesRestartWithoutReleasingTheLock(): Unit = {
    val (service, jdbc) = fixtureWithDb()
    val req = request()
    val outcome = service.execute(1L, req)(() => {
      assertTrue(service.markSmartConfirmationStarted(1L, req.attemptId.get).isRight)
      throw new java.net.SocketTimeoutException("confirmation response lost")
    })
    assertEquals("unknown", outcome.state)
    val restarted = new BookingAttemptService(jdbc, new DataSourceTransactionManager(jdbc.getDataSource),
      new CancellationReceiptService(jdbc))
    assertEquals(0, restarted.recoverPreparedAfterRestart())
    assertTrue(restarted.accountBusy(1L))
    assertEquals("unknown", restarted.status(1L, req.attemptId.get).get.state)
  }

  @Test def thrownPreConfirmationSmartWorkReleasesOnlyPreparedAttempt(): Unit = {
    val service = fixture()
    val before = request()
    val failed = service.execute(1L, before)(() => throw new IllegalArgumentException("pre-confirm conversion"))
    assertEquals("failed", failed.state)
    assertEquals(Some("BOOKING_NOT_SUBMITTED"), failed.errorCode)
    assertFalse(service.accountBusy(1L))
    val after = request()
    val unknown = service.execute(1L, after)(() => {
      assertTrue(service.markSmartConfirmationStarted(1L, after.attemptId.get).isRight)
      throw new java.net.SocketTimeoutException("confirmation response lost")
    })
    assertEquals("unknown", unknown.state)
    assertTrue(service.accountBusy(1L))
  }

  @Test def incompleteLegacyLocktermReleasesPreparedBarrier(): Unit = {
    val service = fixture()
    val api = mock(classOf[ApiService])
    when(api.reservationLockterm(anyLong(), any(), any())).thenReturn(
      Right(ReservationLocktermResponse(Nil, Nil, true, false, null)))
    runLegacyMonitorBooking(service, api)
    assertEquals("clear", service.legacyBarrier(1L).state)
    org.mockito.Mockito.verify(api, org.mockito.Mockito.never()).reservationConfirm(anyLong(), any(), any())
  }

  @Test def thrownPreConfirmationLegacyCallReleasesPreparedBarrier(): Unit = {
    val service = fixture()
    val api = mock(classOf[ApiService])
    when(api.reservationLockterm(anyLong(), any(), any())).thenThrow(new IllegalArgumentException("pre-confirm call"))
    runLegacyMonitorBooking(service, api)
    assertEquals("clear", service.legacyBarrier(1L).state)
    org.mockito.Mockito.verify(api, org.mockito.Mockito.never()).reservationConfirm(anyLong(), any(), any())
  }

  @Test def competingProcessRecoveryPreventsLegacyConfirmation(): Unit = {
    val (service, jdbc) = fixtureWithDb()
    val restarted = new BookingAttemptService(jdbc, new DataSourceTransactionManager(jdbc.getDataSource),
      new CancellationReceiptService(jdbc))
    val api = mock(classOf[ApiService])
    val controller = controllerWith(service, mock(classOf[MonitoringService]), api, smartEnrolled = false)
    val valuation = Valuation(None, None, false, false, None, Some(0.0), None, None, None, false, 1)
    val lock = ReservationLocktermResponse(Nil, Nil, false, false,
      ReservationLocktermResponseValue(false, None, null, Nil, 123L, List(valuation)))
    when(api.getXsrfToken(anyLong())).thenReturn(Right(XsrfToken("tok", Seq.empty)))
    when(api.reservationLockterm(anyLong(), any(), any())).thenAnswer(_ => {
      assertEquals(1, restarted.recoverPreparedAfterRestart())
      Right(lock)
    })
    val response = controller.bookAppointment(1L, request().copy(attemptId = None))
    assertNotEquals(200, response.getStatusCode.value())
    assertEquals("clear", service.legacyBarrier(1L).state)
    org.mockito.Mockito.verify(api, org.mockito.Mockito.never()).reservationConfirm(anyLong(), any(), any())
    org.mockito.Mockito.verify(api, org.mockito.Mockito.never()).reservationChangeTerm(anyLong(), any(), any())
  }

  @Test def confirmationBusinessErrorWithoutReceiptKeepsAccountLocked(): Unit = {
    val service = fixture()
    assertEquals("unknown", service.execute(1, request())(() => Right(success.copy(hasErrors = true, value = null))).state)
    assertTrue(service.accountBusy(1L))
    assertEquals(Some("ACCOUNT_BUSY"), service.execute(1, request())(() => Right(success)).errorCode)
  }

  @Test def contradictoryErrorWithReservationIdKeepsUnknownAccountLock(): Unit = {
    val service = fixture()
    val outcome = service.execute(1, request())(() => Right(success.copy(hasErrors = true)))
    assertEquals("unknown", outcome.state)
    assertTrue(service.accountBusy(1L))
    val next = service.execute(1, request())(() => throw new AssertionError("must not submit another booking"))
    assertEquals(Some("ACCOUNT_BUSY"), next.errorCode)
  }

  @Test def rejectionErrorAfterConfirmationStartsCannotReleaseAccountLock(): Unit = {
    val service = fixture()
    val api = mock(classOf[ApiService])
    val monitorings = mock(classOf[MonitoringService])
    when(monitorings.getActiveMonitorings(1L)).thenReturn(Seq.empty)
    val controller = controllerWith(service, monitorings, api)
    val valuation = Valuation(None, None, false, false, None, Some(0.0), None, None, None, false, 1)
    val lock = ReservationLocktermResponse(Nil, Nil, false, false,
      ReservationLocktermResponseValue(false, None, null, Nil, 123L, List(valuation)))
    when(api.getXsrfToken(anyLong())).thenReturn(Right(XsrfToken("tok", Seq.empty)))
    when(api.reservationLockterm(anyLong(), any(), any())).thenReturn(Right(lock))
    when(api.reservationConfirm(anyLong(), any(), any()))
      .thenReturn(Left(new BookingRejectedException("UPSTREAM_REJECTION_AFTER_SEND")))
    val outcome = controller.submitBookingAttempt(1L, request().copy(isImpediment = Some(false)))
      .getBody.asInstanceOf[ApiResponse[BookingOutcome]].data.get
    assertEquals("unknown", outcome.state)
    assertTrue(service.accountBusy(1L))
  }

  @Test def attemptCannotBeReusedWithAnotherSlot(): Unit = {
    val service = fixture()
    val req = request()
    service.execute(1, req)(() => Right(success))
    assertThrows(classOf[IllegalArgumentException], () => service.execute(1, req.copy(scheduleId = 99))(() => Right(success)))
  }

  @Test def failureBeforeSubmissionReleasesTheAccount(): Unit = {
    val service = fixture()
    val outcome = service.execute(1, request())(() => Left(new BookingNotSubmittedException(new RuntimeException("login failed"))))
    assertEquals("failed", outcome.state)
    assertEquals(Some("BOOKING_NOT_SUBMITTED"), outcome.errorCode)
    assertEquals("succeeded", service.execute(1, request())(() => Right(success)).state)
  }

  @Test def activeLegacyAutomaticMonitorProducesDurableFailureBeforeLuxmedSubmission(): Unit = {
    val service = fixture()
    val monitorings = mock(classOf[MonitoringService])
    val legacy = new Monitoring()
    legacy.autobook = true
    when(monitorings.getActiveMonitorings(1L)).thenReturn(Seq(legacy))
    val controller = controllerWith(service, monitorings)
    val req = request().copy(isImpediment = Some(false))
    val outcome = controller.submitBookingAttempt(1L, req).getBody.asInstanceOf[ApiResponse[BookingOutcome]].data.get
    assertEquals("failed", outcome.state)
    assertEquals(Some("LEGACY_AUTO_MONITOR_ACTIVE"), outcome.errorCode)
    assertEquals(outcome, service.status(1L, req.attemptId.get).get)
    assertFalse(service.accountBusy(1L))
  }

  @Test def monitorReadFailureBeforeSubmissionDoesNotHoldTheAccount(): Unit = {
    val service = fixture()
    val monitorings = mock(classOf[MonitoringService])
    when(monitorings.getActiveMonitorings(1L)).thenThrow(new RuntimeException("fixture database outage"))
    val controller = controllerWith(service, monitorings)
    val req = request().copy(isImpediment = Some(false))
    val outcome = controller.submitBookingAttempt(1L, req).getBody.asInstanceOf[ApiResponse[BookingOutcome]].data.get
    assertEquals("failed", outcome.state)
    assertEquals(Some("BOOKING_NOT_SUBMITTED"), outcome.errorCode)
    assertEquals(outcome, service.status(1L, req.attemptId.get).get)
    assertFalse(service.accountBusy(1L))
  }

  @Test def legacyBookingBarrierBlocksSmartSubmissionUntilItsReceiptIsAcknowledged(): Unit = {
    val service = fixture()
    val monitorings = mock(classOf[MonitoringService])
    when(monitorings.getActiveMonitorings(1L)).thenReturn(Seq.empty)
    val controller = controllerWith(service, monitorings)
    val id = service.beginLegacyBooking(1L, 1791288000000L)
    assertEquals(LegacyBookingBarrier("pending", None, Some(1791288000000L)), service.legacyBarrier(1L))
    val pending = controller.submitBookingAttempt(1L, request().copy(isImpediment = Some(false)))
      .getBody.asInstanceOf[ApiResponse[BookingOutcome]].data.get
    assertEquals(Some("LEGACY_BOOKING_BARRIER"), pending.errorCode)
    service.completeLegacyBooking(1L, id, 77L)
    assertEquals(LegacyBookingBarrier("succeeded", Some(77L), Some(1791288000000L)), service.legacyBarrier(1L))
    val succeeded = controller.submitBookingAttempt(1L, request().copy(isImpediment = Some(false)))
      .getBody.asInstanceOf[ApiResponse[BookingOutcome]].data.get
    assertEquals(Some("LEGACY_BOOKING_BARRIER"), succeeded.errorCode)
    assertEquals(409, controller.acknowledgeLegacyBooking(1L, LegacyBookingAcknowledgeRequest(78L)).getStatusCode.value())
    assertEquals("succeeded", service.legacyBarrier(1L).state)
    assertEquals(200, controller.acknowledgeLegacyBooking(1L, LegacyBookingAcknowledgeRequest(77L)).getStatusCode.value())
    assertEquals("clear", service.legacyBarrier(1L).state)
    assertEquals("clear", service.legacyBarrier(2L).state)
  }

  @Test def pendingLegacyBarrierCanOnlyBeClearedBeforeSubmission(): Unit = {
    val service = fixture()
    val early = service.beginLegacyBooking(1L, 1791288000000L)
    service.clearLegacyBeforeSubmission(1L, early)
    assertEquals("clear", service.legacyBarrier(1L).state)
    val unknown = service.beginLegacyBooking(1L, 1791288000000L)
    assertFalse(service.acknowledgeLegacyBooking(1L, 77L))
    assertEquals("pending", service.legacyBarrier(1L).state)
    service.completeLegacyBooking(1L, unknown, 77L)
    val second = service.beginLegacyBooking(1L, 1791374400000L)
    assertEquals("pending", service.legacyBarrier(1L).state)
    assertTrue(service.acknowledgeLegacyBooking(1L, 77L))
    assertEquals("pending", service.legacyBarrier(1L).state)
    service.clearLegacyBeforeSubmission(1L, second)
    assertEquals("clear", service.legacyBarrier(1L).state)
  }

  @Test def pendingLegacyOutcomeBlocksAllMonitorsButSuccessBlocksOnlyItsSource(): Unit = {
    val service = fixture()
    val id = service.beginLegacyBooking(1L, 1791288000000L, Some(10L))
    assertFalse(service.canLegacyMonitorBook(1L, 10L))
    assertFalse(service.canLegacyMonitorBook(1L, 11L))
    service.completeLegacyBooking(1L, id, 77L)
    assertFalse(service.canLegacyMonitorBook(1L, 10L))
    assertTrue(service.canLegacyMonitorBook(1L, 11L))
    assertTrue(service.canLegacyMonitorBook(2L, 10L))
  }

  @Test def oldChatCallbackEntersAccountFenceBeforeAnyBooking(): Unit = {
    val service = fixture()
    class RecordingFence extends AccountBookingFence {
      var calls = 0
      override def withLock[A](accountId: Long)(body: => A): A = {
        calls += 1
        super.withLock(accountId)(body)
      }
    }
    val fence = new RecordingFence()
    val monitoring = new Monitoring()
    monitoring.recordId = 10L
    monitoring.accountId = 1L
    monitoring.cityId = 1L
    monitoring.clinicId = 2L
    monitoring.serviceId = 3L
    monitoring.doctorId = 4L
    monitoring.dateFrom = ZonedDateTime.of(2026, 6, 1, 0, 0, 0, 0, ZoneId.of("Europe/Warsaw"))
    monitoring.dateTo = monitoring.dateFrom.plusDays(1)
    monitoring.timeFrom = LocalTime.of(8, 0)
    monitoring.timeTo = LocalTime.of(18, 0)
    monitoring.active = false // The account fence is still entered before this live-state check.
    val data = mock(classOf[DataService])
    when(data.findMonitoring(anyLong(), anyLong())).thenReturn(Some(monitoring))
    val api = mock(classOf[ApiService])
    val date = LocalDateTime.of(2026, 6, 1, 10, 0)
    val funnyDate = LuxmedFunnyDateTime(dateTimeLocal = Some(date))
    val doctor = Doctor(Some("dr"), Some(List(4L)), Some("John"), Some(false), Some(1L), 4L, Some("Smith"))
    val term = TermExt(AdditionalData(false, Nil),
      Term(Some("Clinic"), 2L, 3L, funnyDate, funnyDate, doctor, Some(""),
        false, false, false, 1L, 1000L, 100L))
    when(api.getAvailableTerms(anyLong(), anyLong(), any(), anyLong(), any(), any(), any(), any(), any(), anyLong()))
      .thenReturn(Right(List(term)))
    val monitorings = new MonitoringService()
    for ((name, value) <- List(
      "dataService" -> data,
      "apiService" -> api,
      "bookingFence" -> fence,
      "bookingAttempts" -> service
    )) {
      val field = classOf[MonitoringService].getDeclaredField(name)
      field.setAccessible(true)
      field.set(monitorings, value)
    }
    val time = java.time.Duration.between(LocalDateTime.of(2022, 1, 1, 0, 0), date).toMinutes
    monitorings.bookAppointmentByScheduleId(1L, 10L, 1000L, time)
    assertEquals(1, fence.calls)
    assertEquals("clear", service.legacyBarrier(1L).state)
  }

  @Test def legacyBookPathRejectsAttemptIdsInsteadOfBypassingSmartChecks(): Unit = {
    val service = fixture()
    val controller = controllerWith(service, mock(classOf[MonitoringService]))
    val req = request()
    assertEquals(400, controller.bookAppointment(1L, req).getStatusCode.value())
    assertTrue(service.status(1L, req.attemptId.get).isEmpty)
  }

  @Test def manualLegacyBookPersistsItsReceiptBeforeReturning(): Unit = {
    val service = fixture()
    val api = mock(classOf[ApiService])
    val controller = controllerWith(service, mock(classOf[MonitoringService]), api, smartEnrolled = false)
    val valuation = Valuation(None, None, false, false, None, Some(0.0), None, None, None, false, 1)
    val lock = ReservationLocktermResponse(Nil, Nil, false, false,
      ReservationLocktermResponseValue(false, None, null, Nil, 123L, List(valuation)))
    when(api.getXsrfToken(anyLong())).thenReturn(Right(XsrfToken("tok", Seq.empty)))
    when(api.reservationLockterm(anyLong(), any(), any())).thenReturn(Right(lock))
    when(api.reservationConfirm(anyLong(), any(), any())).thenReturn(Right(success))
    val response = controller.bookAppointment(1L, request().copy(attemptId = None))
    assertEquals(200, response.getStatusCode.value())
    assertEquals(LegacyBookingBarrier("succeeded", Some(77L), Some(1791280800000L)), service.legacyBarrier(1L))
    assertEquals(409, controller.bookAppointment(1L, request().copy(attemptId = None)).getStatusCode.value())
  }

  @Test def malformedLegacyVisitEndDoesNotLeaveAPreparedBarrier(): Unit = {
    val service = fixture()
    val api = mock(classOf[ApiService])
    val controller = controllerWith(service, mock(classOf[MonitoringService]), api, smartEnrolled = false)
    val invalid = request().copy(attemptId = None, dateTimeTo = "not-a-date")
    assertEquals(500, controller.bookAppointment(1L, invalid).getStatusCode.value())
    assertEquals("clear", service.legacyBarrier(1L).state)
  }

  @Test def thrownPreConfirmationFailureDoesNotLeaveAPreparedBarrier(): Unit = {
    val service = fixture()
    val api = mock(classOf[ApiService])
    val controller = controllerWith(service, mock(classOf[MonitoringService]), api, smartEnrolled = false)
    when(api.getXsrfToken(anyLong())).thenThrow(new IllegalStateException("token unavailable"))
    assertEquals(500, controller.bookAppointment(1L, request().copy(attemptId = None)).getStatusCode.value())
    assertEquals("clear", service.legacyBarrier(1L).state)
  }

  @Test def thrownConfirmationKeepsLegacyOutcomeUnknown(): Unit = {
    val service = fixture()
    val api = mock(classOf[ApiService])
    val controller = controllerWith(service, mock(classOf[MonitoringService]), api, smartEnrolled = false)
    val valuation = Valuation(None, None, false, false, None, Some(0.0), None, None, None, false, 1)
    val lock = ReservationLocktermResponse(Nil, Nil, false, false,
      ReservationLocktermResponseValue(false, None, null, Nil, 123L, List(valuation)))
    when(api.getXsrfToken(anyLong())).thenReturn(Right(XsrfToken("tok", Seq.empty)))
    when(api.reservationLockterm(anyLong(), any(), any())).thenReturn(Right(lock))
    when(api.reservationConfirm(anyLong(), any(), any())).thenThrow(new IllegalStateException("response lost"))
    assertEquals(500, controller.bookAppointment(1L, request().copy(attemptId = None)).getStatusCode.value())
    assertEquals("pending", service.legacyBarrier(1L).state)
  }

  @Test def lostManualLegacyConfirmationKeepsPendingBarrier(): Unit = {
    val service = fixture()
    val api = mock(classOf[ApiService])
    val controller = controllerWith(service, mock(classOf[MonitoringService]), api, smartEnrolled = false)
    val valuation = Valuation(None, None, false, false, None, Some(0.0), None, None, None, false, 1)
    val lock = ReservationLocktermResponse(Nil, Nil, false, false,
      ReservationLocktermResponseValue(false, None, null, Nil, 123L, List(valuation)))
    when(api.getXsrfToken(anyLong())).thenReturn(Right(XsrfToken("tok", Seq.empty)))
    when(api.reservationLockterm(anyLong(), any(), any())).thenReturn(Right(lock))
    when(api.reservationConfirm(anyLong(), any(), any()))
      .thenReturn(Left(new java.net.SocketTimeoutException("lost response")))
    controller.bookAppointment(1L, request().copy(attemptId = None))
    assertEquals("pending", service.legacyBarrier(1L).state)
    assertEquals(409, controller.bookAppointment(1L, request().copy(attemptId = None)).getStatusCode.value())
  }

  @Test def incompleteManualLegacyReceiptIsNotReportedAsBooked(): Unit = {
    val service = fixture()
    val api = mock(classOf[ApiService])
    val controller = controllerWith(service, mock(classOf[MonitoringService]), api, smartEnrolled = false)
    val valuation = Valuation(None, None, false, false, None, Some(0.0), None, None, None, false, 1)
    val lock = ReservationLocktermResponse(Nil, Nil, false, false,
      ReservationLocktermResponseValue(false, None, null, Nil, 123L, List(valuation)))
    when(api.getXsrfToken(anyLong())).thenReturn(Right(XsrfToken("tok", Seq.empty)))
    when(api.reservationLockterm(anyLong(), any(), any())).thenReturn(Right(lock))
    when(api.reservationConfirm(anyLong(), any(), any()))
      .thenReturn(Right(success.copy(hasErrors = true)))
    assertEquals(409, controller.bookAppointment(1L, request().copy(attemptId = None)).getStatusCode.value())
    assertEquals("pending", service.legacyBarrier(1L).state)
  }

  @Test def enrollmentPermanentlyFencesOldManualBookPath(): Unit = {
    val service = fixture()
    val controller = controllerWith(service, mock(classOf[MonitoringService]), smartEnrolled = false)
    assertFalse(service.smartBookingEnrolled(1L))
    assertEquals(200, controller.enrollSmartBooking(1L, SmartBookingEnrollmentRequest(Nil)).getStatusCode.value())
    assertEquals(200, controller.enrollSmartBooking(1L, SmartBookingEnrollmentRequest(Nil)).getStatusCode.value())
    assertTrue(service.smartBookingEnrolled(1L))
    assertEquals(409, controller.bookAppointment(1L, request().copy(attemptId = None)).getStatusCode.value())
    assertFalse(service.canLegacyMonitorBook(1L, 99L))
    assertEquals("clear", service.legacyBarrier(1L).state)
  }

  @Test def enrollmentCapabilityRequiresThePreviewedMonitorSet(): Unit = {
    val controller = controllerWith(fixture(), mock(classOf[MonitoringService]), smartEnrolled = false)
    val capabilities = controller.capabilities().getBody.asInstanceOf[ApiResponse[List[String]]].data.get
    assertTrue(capabilities.contains("smart-booking-enrollment-fence-v2"))
  }

  @Test def enrollmentRequiresTheExactPreviewedAutomaticMonitorSet(): Unit = {
    val (service, jdbc) = fixtureWithDb()
    jdbc.update("INSERT INTO monitoring(record_id,account_id,active,autobook) VALUES (10,1,TRUE,TRUE)")
    jdbc.update("INSERT INTO monitoring(record_id,account_id,active,autobook) VALUES (11,1,TRUE,TRUE)")
    jdbc.update("INSERT INTO monitoring(record_id,account_id,active,autobook) VALUES (12,1,TRUE,FALSE)")
    jdbc.update("INSERT INTO monitoring(record_id,account_id,active,autobook) VALUES (13,2,TRUE,TRUE)")
    val monitorings = mock(classOf[MonitoringService])
    val controller = controllerWith(service, monitorings, smartEnrolled = false)
    assertEquals(409, controller.enrollSmartBooking(1L, SmartBookingEnrollmentRequest(List(10L))).getStatusCode.value())
    assertFalse(service.smartBookingEnrolled(1L))
    assertEquals(2L, jdbc.queryForObject("SELECT COUNT(*) FROM monitoring WHERE account_id=1 AND active=TRUE AND autobook=TRUE", classOf[java.lang.Long]))
    assertEquals(409, controller.enrollSmartBooking(1L, SmartBookingEnrollmentRequest(List(11L, 10L))).getStatusCode.value())
    assertFalse(service.smartBookingEnrolled(1L))

    val response = controller.enrollSmartBooking(1L, SmartBookingEnrollmentRequest(List(10L, 11L)))
    assertEquals(200, response.getStatusCode.value())
    val enrolled = response.getBody.asInstanceOf[ApiResponse[SmartBookingEnrollment]].data.get
    assertEquals(Seq(10L, 11L), enrolled.stoppedAutoMonitorIds)
    assertTrue(service.smartBookingEnrolled(1L))
    assertFalse(service.canLegacyMonitorBook(1L, 10L))
    assertEquals(0L, jdbc.queryForObject("SELECT COUNT(*) FROM monitoring WHERE account_id=1 AND active=TRUE AND autobook=TRUE", classOf[java.lang.Long]))
    assertEquals(1L, jdbc.queryForObject("SELECT COUNT(*) FROM monitoring WHERE account_id=1 AND active=TRUE AND autobook=FALSE", classOf[java.lang.Long]))
    assertEquals(1L, jdbc.queryForObject("SELECT COUNT(*) FROM monitoring WHERE account_id=2 AND active=TRUE AND autobook=TRUE", classOf[java.lang.Long]))
  }

  @Test def enrolledAccountCannotCreateAnotherAutomaticMonitor(): Unit = {
    val service = fixture()
    service.enrollSmartBooking(1L)
    val monitoringService = new MonitoringService()
    for ((name, value) <- List(
      "bookingFence" -> new AccountBookingFence(),
      "bookingAttempts" -> service
    )) {
      val field = classOf[MonitoringService].getDeclaredField(name)
      field.setAccessible(true)
      field.set(monitoringService, value)
    }
    val monitoring = new Monitoring()
    monitoring.accountId = 1L
    monitoring.autobook = true
    val rejected = assertThrows(classOf[BookingRejectedException], () => monitoringService.createMonitoring(monitoring))
    assertEquals("SMART_BOOKING_ENROLLED", rejected.code)
  }

  @Test def smartAttemptNeedsDurableEnrollmentMarker(): Unit = {
    val service = fixture()
    val controller = controllerWith(service, mock(classOf[MonitoringService]), smartEnrolled = false)
    val req = request().copy(isImpediment = Some(false))
    val outcome = controller.submitBookingAttempt(1L, req).getBody.asInstanceOf[ApiResponse[BookingOutcome]].data.get
    assertEquals(Some("SMART_BOOKING_NOT_ENROLLED"), outcome.errorCode)
    assertFalse(service.accountBusy(1L))
  }

  @Test def duplicateNormalizedLuxmedIdentityCannotBeEnrolled(): Unit = {
    val (service, jdbc) = fixtureWithDb()
    jdbc.update("INSERT INTO credentials(account_id,username) VALUES (?,?)", Long.box(2L), " PERSON@EXAMPLE.COM ")
    val controller = controllerWith(service, mock(classOf[MonitoringService]), smartEnrolled = false)
    assertEquals(409, controller.enrollSmartBooking(1L, SmartBookingEnrollmentRequest(Nil)).getStatusCode.value())
    assertFalse(service.smartBookingEnrolled(1L))
    assertFalse(service.smartBookingIdentitySafe(1L))
  }

  @Test def aliasCreatedAfterEnrollmentCannotBookOrPreserveSmartEligibility(): Unit = {
    val (service, jdbc) = fixtureWithDb()
    val controller = controllerWith(service, mock(classOf[MonitoringService]))
    jdbc.update("INSERT INTO credentials(account_id,username) VALUES (?,?)", Long.box(2L), "PERSON@example.com")
    assertFalse(service.smartBookingIdentitySafe(1L))
    assertTrue(service.identityHasSmartEnrollment(2L))
    assertFalse(service.canLegacyMonitorBook(2L, 10L))
    assertEquals(409, controller.bookAppointment(2L, request().copy(attemptId = None)).getStatusCode.value())
    val outcome = controller.submitBookingAttempt(1L, request().copy(isImpediment = Some(false)))
      .getBody.asInstanceOf[ApiResponse[BookingOutcome]].data.get
    assertEquals(Some("SMART_BOOKING_IDENTITY_UNSAFE"), outcome.errorCode)
  }

  @Test def enrolledAccountRemainsFencedAfterItsUsernameChanges(): Unit = {
    val (service, jdbc) = fixtureWithDb()
    val controller = controllerWith(service, mock(classOf[MonitoringService]))
    jdbc.update("UPDATE credentials SET username=? WHERE account_id=?", "other@example.com", Long.box(1L))
    assertTrue(service.identityHasSmartEnrollment(1L))
    assertFalse(service.smartBookingIdentitySafe(1L))
    assertEquals(409, controller.enrollSmartBooking(1L, SmartBookingEnrollmentRequest(Nil)).getStatusCode.value())
    assertEquals(409, controller.bookAppointment(1L, request().copy(attemptId = None)).getStatusCode.value())
  }

  @Test def newChatCannotLinkAnAlreadyEnrolledLuxmedIdentity(): Unit = {
    val service = fixture()
    service.enrollSmartBooking(1L)
    val data = new DataService()
    val repository = mock(classOf[DataRepository])
    when(repository.findUserIdBySource("new-chat", 3L)).thenReturn(None)
    for ((name, value) <- List("bookingAttempts" -> service, "dataRepository" -> repository)) {
      val field = classOf[DataService].getDeclaredField(name)
      field.setAccessible(true)
      field.set(data, value)
    }
    val source = MessageSource(MessageSourceSystem(3L), "new-chat")
    assertThrows(classOf[BookingRejectedException], () => data.saveCredentials(source, "PERSON@EXAMPLE.COM", "encrypted"))
  }

  @Test def earlyValidationFailuresHaveRecoverableReceipts(): Unit = {
    val service = fixture()
    val controller = controllerWith(service, mock(classOf[MonitoringService]))
    val impeded = request()
    val first = controller.submitBookingAttempt(1L, impeded).getBody.asInstanceOf[ApiResponse[BookingOutcome]].data.get
    assertEquals(Some("IMPEDIMENT_REQUIRES_REVIEW"), first.errorCode)
    assertEquals(first, service.status(1L, impeded.attemptId.get).get)
    val unprepared = request().copy(isImpediment = Some(false), isPreparationRequired = true)
    val second = controller.submitBookingAttempt(1L, unprepared).getBody.asInstanceOf[ApiResponse[BookingOutcome]].data.get
    assertEquals(Some("PREPARATION_MISSING"), second.errorCode)
    assertEquals(second, service.status(1L, unprepared.attemptId.get).get)
    assertFalse(service.accountBusy(1L))
  }

  @Test def smartReplacementIsRejectedBeforeAnyProviderBookingCall(): Unit = {
    val service = fixture()
    val api = mock(classOf[ApiService])
    val controller = controllerWith(service, mock(classOf[MonitoringService]), api)
    val replacement = request().copy(isImpediment = Some(false), rebookIfExists = true)
    val outcome = controller.submitBookingAttempt(1L, replacement)
      .getBody.asInstanceOf[ApiResponse[BookingOutcome]].data.get
    assertEquals("failed", outcome.state)
    assertEquals(Some("SMART_REBOOK_REQUIRES_REVIEW"), outcome.errorCode)
    assertEquals(outcome, service.status(1L, replacement.attemptId.get).get)
    assertFalse(service.accountBusy(1L))
    org.mockito.Mockito.verify(api, org.mockito.Mockito.never()).getXsrfToken(anyLong())
    org.mockito.Mockito.verify(api, org.mockito.Mockito.never()).reservationLockterm(anyLong(), any(), any())
    org.mockito.Mockito.verify(api, org.mockito.Mockito.never()).reservationConfirm(anyLong(), any(), any())
    org.mockito.Mockito.verify(api, org.mockito.Mockito.never()).reservationChangeTerm(anyLong(), any(), any())
  }

  @Test def lockBusinessErrorsAndUnapprovedReplacementPreventConfirmation(): Unit = {
    val value = ReservationLocktermResponseValue(true, None, null, Nil, 123, Nil)
    val response = ReservationLocktermResponse(Nil, Nil, false, false, value)
    assertTrue(BookingAttemptService.validateLockterm(response, false).isLeft)
    assertTrue(BookingAttemptService.validateLockterm(response, true).isLeft)
    val valuation = Valuation(None, None, false, false, None, Some(0.0), None, None, None, false, 1)
    val withValuation = response.copy(value = value.copy(valuations = List(valuation)))
    assertTrue(BookingAttemptService.validateLockterm(withValuation.copy(value = withValuation.value.copy(valuations = List(valuation.copy(price = Some(25.0))))), false).isLeft)
    assertTrue(BookingAttemptService.validateLockterm(withValuation.copy(value = withValuation.value.copy(valuations = List(valuation.copy(isReferralRequired = true)))), false).isLeft)
    assertTrue(BookingAttemptService.validateLockterm(withValuation, true).isLeft)
    assertTrue(BookingAttemptService.validateLockterm(withValuation.copy(value = withValuation.value.copy(changeTermAvailable = false)), false).isRight)
    val related = RelatedVisit(null, "Clinic", false, 77, LocalTime.NOON, LocalTime.NOON.plusMinutes(30))
    assertTrue(BookingAttemptService.validateLockterm(withValuation.copy(value = withValuation.value.copy(relatedVisits = List(related))), true).isRight)
    assertTrue(BookingAttemptService.validateLockterm(response.copy(hasErrors = true), true).isLeft)
    assertTrue(BookingAttemptService.validateLockterm(response.copy(value = null), true).isLeft)
    val service = fixture()
    val outcome = service.execute(1, request())(() => Left(new BookingRejectedException("ALREADY_RESERVED")))
    assertEquals(Some("ALREADY_RESERVED"), outcome.errorCode)
    assertEquals("succeeded", service.execute(1, request())(() => Right(success)).state)
  }
}
