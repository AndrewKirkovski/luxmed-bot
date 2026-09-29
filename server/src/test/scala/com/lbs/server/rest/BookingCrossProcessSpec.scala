package com.lbs.server.rest

import com.lbs.api.json.model.ReservationConfirmRequest
import com.lbs.server.service.{AccountBookingFence, ApiService, CancellationReceiptService}
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import org.springframework.jdbc.core.JdbcTemplate
import org.springframework.jdbc.datasource.{DataSourceTransactionManager, DriverManagerDataSource}

import java.util.UUID
import java.time.LocalTime
import java.util.concurrent.{Callable, CountDownLatch, Executors, TimeUnit}
import scala.util.Try

class BookingCrossProcessSpec {
  private def fixture(): (DriverManagerDataSource, JdbcTemplate) = {
    val source = new DriverManagerDataSource(s"jdbc:h2:mem:${UUID.randomUUID()};DB_CLOSE_DELAY=-1", "sa", "")
    val jdbc = new JdbcTemplate(source)
    jdbc.execute("CREATE TABLE booking_attempt(id VARCHAR(36) PRIMARY KEY, account_id BIGINT NOT NULL, fingerprint VARCHAR(64) NOT NULL, state VARCHAR(20) NOT NULL, reservation_id BIGINT, error_code VARCHAR(64), created_at BIGINT NOT NULL, phase VARCHAR(24), process_id VARCHAR(36), start_at BIGINT, end_at BIGINT, clinic_id BIGINT, service_id BIGINT, schedule_id BIGINT, doctor_id BIGINT, telemedicine BOOLEAN, baseline_reservation_ids TEXT, baseline_reservation_facts TEXT, acknowledged_at BIGINT)")
    jdbc.execute("CREATE TABLE booking_account_lock(account_id BIGINT PRIMARY KEY, attempt_id VARCHAR(36) NOT NULL)")
    jdbc.execute("CREATE TABLE legacy_booking_barrier(id VARCHAR(36) PRIMARY KEY, account_id BIGINT NOT NULL, state VARCHAR(20) NOT NULL, reservation_id BIGINT, monitoring_id BIGINT, start_at BIGINT NOT NULL, created_at BIGINT NOT NULL, phase VARCHAR(24), process_id VARCHAR(36))")
    jdbc.execute("CREATE INDEX legacy_booking_barrier_account_idx ON legacy_booking_barrier(account_id)")
    jdbc.execute("CREATE TABLE smart_booking_enrollment(account_id BIGINT PRIMARY KEY, identity_key VARCHAR(255) NOT NULL UNIQUE, enrolled_at BIGINT NOT NULL)")
    jdbc.execute("CREATE TABLE monitoring(record_id BIGINT PRIMARY KEY, account_id BIGINT NOT NULL, active BOOLEAN NOT NULL, autobook BOOLEAN NOT NULL)")
    jdbc.execute("CREATE TABLE credentials(account_id BIGINT NOT NULL, username VARCHAR(255) NOT NULL)")
    jdbc.execute("CREATE TABLE cancellation_receipt(account_id BIGINT NOT NULL, reservation_id BIGINT NOT NULL, start_at BIGINT NOT NULL, state VARCHAR(20) NOT NULL, requested_at BIGINT NOT NULL, confirmed_at BIGINT, PRIMARY KEY(account_id,reservation_id))")
    jdbc.execute("ALTER TABLE cancellation_receipt ADD COLUMN moved_acknowledged_at BIGINT")
    jdbc.update("INSERT INTO credentials(account_id,username) VALUES (1,'person@example.com')")
    (source, jdbc)
  }

  @Test def twoSidecarInstancesCannotPrepareLegacyBookingsForOneAccount(): Unit = {
    val (source, jdbc) = fixture()
    val services = Seq.fill(2)(new BookingAttemptService(jdbc, new DataSourceTransactionManager(source),
      new CancellationReceiptService(jdbc)))
    val fences = Seq.fill(2)(new AccountBookingFence())
    val ready = new CountDownLatch(2)
    val release = new CountDownLatch(1)
    val pool = Executors.newFixedThreadPool(2)
    try {
      val results = services.zip(fences).map { case (service, fence) =>
        pool.submit(new Callable[Try[String]] {
          override def call(): Try[String] = fence.withLock(1L) {
            // Both sidecar instances have passed the controller's read-only checks.
            val allowed = !service.identityHasSmartEnrollment(1L) && !service.accountBusy(1L) &&
              service.legacyBarrier(1L).state == "clear"
            ready.countDown()
            if (!release.await(5, TimeUnit.SECONDS)) throw new AssertionError("release latch timed out")
            if (!allowed) throw new AssertionError("fixture did not reach the legacy booking race")
            Try(service.beginLegacyBooking(1L, 1791280800000L))
          }
        })
      }
      assertTrue(ready.await(5, TimeUnit.SECONDS), "both instances must pass their prechecks")
      release.countDown()
      val obtained = results.map(_.get(5, TimeUnit.SECONDS)).count(_.isSuccess)
      assertEquals(1, obtained, "only one instance may receive permission to confirm with LuxMed")
      assertEquals(1L, jdbc.queryForObject(
        "SELECT COUNT(*) FROM legacy_booking_barrier WHERE account_id=? AND state='pending'",
        classOf[java.lang.Long], Long.box(1L)).longValue())
    } finally {
      release.countDown()
      pool.shutdownNow()
    }
  }

  @Test def cancellationCannotStartAfterAnotherInstanceOwnsSmartBookingPermit(): Unit = {
    val (source, jdbc) = fixture()
    val smart = new BookingAttemptService(jdbc, new DataSourceTransactionManager(source),
      new CancellationReceiptService(jdbc))
    val cancellation = new BookingAttemptService(jdbc, new DataSourceTransactionManager(source),
      new CancellationReceiptService(jdbc))
    val entered = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val pool = Executors.newSingleThreadExecutor()
    val request = BookRequest(1, 2, 2, "Clinic", 3, "First", "Last", "", 4, 5, 6,
      "2026-10-06T12:00:00", "2026-10-06T12:30:00", attemptId = Some(UUID.randomUUID().toString),
      baselineReservationIds = Some(Nil), baselineReservations = Some(Nil))
    try {
      val booking = pool.submit(new Callable[BookingOutcome] {
        override def call(): BookingOutcome = smart.execute(1L, request)(() => {
          entered.countDown()
          if (!release.await(5, TimeUnit.SECONDS)) throw new AssertionError("booking release timed out")
          Left(new java.net.SocketTimeoutException("provider outcome unknown"))
        })
      })
      assertTrue(entered.await(5, TimeUnit.SECONDS), "smart booking must hold its permit")
      assertThrows(classOf[BookingRejectedException], () => cancellation.beginCancellation(1L, 77L, 1791280800000L))
      assertFalse(new CancellationReceiptService(jdbc).hasPending(1L), "no cancellation may reach DELETE while booking is unresolved")
      release.countDown()
      assertEquals("unknown", booking.get(5, TimeUnit.SECONDS).state)
    } finally {
      release.countDown()
      pool.shutdownNow()
    }
  }

  @Test def smartBookingCannotStartAfterAnotherInstanceOwnsCancellationPermit(): Unit = {
    val (source, jdbc) = fixture()
    val cancellation = new BookingAttemptService(jdbc, new DataSourceTransactionManager(source),
      new CancellationReceiptService(jdbc))
    val smart = new BookingAttemptService(jdbc, new DataSourceTransactionManager(source),
      new CancellationReceiptService(jdbc))
    cancellation.beginCancellation(1L, 77L, 1791280800000L)
    val request = BookRequest(1, 2, 2, "Clinic", 3, "First", "Last", "", 4, 5, 6,
      "2026-10-06T12:00:00", "2026-10-06T12:30:00", attemptId = Some(UUID.randomUUID().toString),
      baselineReservationIds = Some(Nil), baselineReservations = Some(Nil))
    var providerCalls = 0
    val outcome = smart.execute(1L, request)(() => {
      providerCalls += 1
      Left(new java.net.SocketTimeoutException("provider outcome unknown"))
    })
    assertEquals(0, providerCalls, "a pending cancellation must block provider confirmation")
    assertEquals("failed", outcome.state)
    assertTrue(smart.accountBusy(1L))
  }

  @Test def chatConfirmationCannotBorrowAnotherInstancesSmartPermit(): Unit = {
    val (source, jdbc) = fixture()
    val smart = new BookingAttemptService(jdbc, new DataSourceTransactionManager(source),
      new CancellationReceiptService(jdbc))
    val chat = new BookingAttemptService(jdbc, new DataSourceTransactionManager(source),
      new CancellationReceiptService(jdbc))
    val api = new ApiService()
    for ((name, value) <- List(
      "bookingFence" -> new AccountBookingFence(),
      "bookingAttempts" -> chat,
      "cancellationReceipts" -> new CancellationReceiptService(jdbc)
    )) {
      val field = classOf[ApiService].getDeclaredField(name)
      field.setAccessible(true)
      field.set(api, value)
    }
    val request = BookRequest(1, 2, 2, "Clinic", 3, "First", "Last", "", 4, 5, 6,
      "2026-10-06T12:00:00", "2026-10-06T12:30:00", attemptId = Some(UUID.randomUUID().toString),
      baselineReservationIds = Some(Nil), baselineReservations = Some(Nil))
    val entered = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val pool = Executors.newSingleThreadExecutor()
    try {
      val booking = pool.submit(new Callable[BookingOutcome] {
        override def call(): BookingOutcome = smart.execute(1L, request)(() => {
          assertTrue(smart.markSmartConfirmationStarted(1L, request.attemptId.get).isRight)
          entered.countDown()
          if (!release.await(5, TimeUnit.SECONDS)) throw new AssertionError("booking release timed out")
          Left(new java.net.SocketTimeoutException("provider outcome unknown"))
        })
      })
      assertTrue(entered.await(5, TimeUnit.SECONDS))
      val chatRequest = ReservationConfirmRequest("2026-10-06T12:00:00Z", 3L, 2L, 4L, 5L, 6L,
        7L, LocalTime.NOON, null)
      assertEquals("ACCOUNT_BUSY", api.reservationConfirm(1L, null, chatRequest)
        .left.toOption.get.asInstanceOf[BookingRejectedException].code)
      assertEquals("BOOKING_PERMIT_REQUIRED", api.reservationConfirm(1L, null, chatRequest, request.attemptId.get)
        .left.toOption.get.asInstanceOf[BookingRejectedException].code)
      release.countDown()
      assertEquals("unknown", booking.get(5, TimeUnit.SECONDS).state)
    } finally {
      release.countDown()
      pool.shutdownNow()
    }
  }

  @Test def automaticMonitorWriteAndEnrollmentShareADurableAccountPermit(): Unit = {
    val (source, jdbc) = fixture()
    val writing = new BookingAttemptService(jdbc, new DataSourceTransactionManager(source),
      new CancellationReceiptService(jdbc))
    val enrolling = new BookingAttemptService(jdbc, new DataSourceTransactionManager(source),
      new CancellationReceiptService(jdbc))
    val entered = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val pool = Executors.newFixedThreadPool(2)
    try {
      val write = pool.submit(new Callable[Unit] {
        override def call(): Unit = writing.withAutoMonitorWrite(1L) {
          jdbc.update("INSERT INTO monitoring(record_id,account_id,active,autobook) VALUES (10,1,TRUE,TRUE)")
          entered.countDown()
          if (!release.await(5, TimeUnit.SECONDS)) throw new AssertionError("monitor release timed out")
          ()
        }
      })
      assertTrue(entered.await(5, TimeUnit.SECONDS))
      val enrollment = pool.submit(new Callable[Try[Seq[Long]]] {
        override def call(): Try[Seq[Long]] = Try(enrolling.enrollSmartBooking(1L, Nil))
      })
      Thread.sleep(100)
      assertFalse(enrollment.isDone, "enrollment must wait for the monitor-write transaction")
      release.countDown()
      write.get(5, TimeUnit.SECONDS)
      assertEquals("SMART_MONITORS_CHANGED", enrollment.get(5, TimeUnit.SECONDS).failed.get
        .asInstanceOf[BookingRejectedException].code)
      assertEquals(Seq(10L), enrolling.enrollSmartBooking(1L, Seq(10L)))
      assertEquals(false, jdbc.queryForObject("SELECT active FROM monitoring WHERE record_id=10", classOf[java.lang.Boolean]))
    } finally {
      release.countDown()
      pool.shutdownNow()
    }
  }
}
