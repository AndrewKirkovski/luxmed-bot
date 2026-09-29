package com.lbs.server.rest

import com.lbs.api.json.model.{Event, ReservationConfirmResponse, ReservationLocktermResponse}
import org.springframework.dao.DuplicateKeyException
import org.springframework.jdbc.core.{JdbcTemplate, RowMapper}
import org.springframework.stereotype.Service
import org.springframework.transaction.PlatformTransactionManager
import org.springframework.transaction.support.TransactionTemplate
import com.lbs.server.service.{CancellationReceiptService, MovedReservationFact}
import jakarta.annotation.PostConstruct
import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest
import java.time.{LocalDateTime, OffsetDateTime, ZoneId}
import java.time.format.DateTimeParseException
import java.util.UUID
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

class BookingNotSubmittedException(cause: Throwable) extends RuntimeException("Booking was not submitted", cause)
class BookingRejectedException(val code: String) extends RuntimeException(code)
case class LegacyBookingBarrier(state: String, reservationId: Option[Long] = None, start: Option[Long] = None,
                                id: Option[String] = None)
case class LegacyBookingAcknowledgeRequest(reservationId: Long, id: Option[String] = None,
                                           expectedStartAt: Option[Long] = None)
case class SmartBookingAcknowledgeRequest(reservationId: Long)
case class BookingRecoveryContext(attemptId: String, state: String, phase: Option[String], fingerprint: String,
                                  startAt: Long, endAt: Long, clinicId: Long, serviceId: Long,
                                  scheduleId: Long, doctorId: Long, telemedicine: Boolean,
                                  baselineReservationIds: List[Long])
case class BookingPositiveReviewRequest(reservationId: Long, expectedFingerprint: String,
                                        expectedStartAt: Long, expectedEndAt: Long,
                                        expectedClinicId: Long, expectedBaselineReservationIds: List[Long],
                                        serviceAndDoctorVerified: Boolean, providerRequestSettled: Boolean,
                                        operator: String, reason: String)
case class SmartBookingEnrollment(enrolled: Boolean, stoppedAutoMonitorIds: Seq[Long] = Nil)
case class SmartBookingEnrollmentRequest(expectedAutoMonitorIds: List[Long])

object BookingAttemptService {
  private[rest] def fingerprint(value: String): String =
    MessageDigest.getInstance("SHA-256").digest(value.getBytes(UTF_8)).map(b => f"${b & 0xff}%02x").mkString

  /** The previous BookRequest case class had 21 fields and no baseline. */
  private[rest] def legacyFingerprint(request: BookRequest): String =
    fingerprint(request.copy(attemptId = None).productIterator.take(21).mkString("BookRequest(", ",", ")"))

  /** v3 requests had IDs but no reservation facts. They remain replayable after upgrade. */
  private[rest] def v3Fingerprint(request: BookRequest): String =
    fingerprint(request.copy(attemptId = None).productIterator.take(22).mkString("BookRequest(", ",", ")"))

  private def instant(value: String): Long = {
    try OffsetDateTime.parse(value).toInstant.toEpochMilli
    catch { case _: DateTimeParseException =>
      LocalDateTime.parse(value).atZone(ZoneId.of("Europe/Warsaw")).toInstant.toEpochMilli
    }
  }

  private[rest] def recoveryFacts(request: BookRequest): Option[(Long, Long, List[Long])] =
    Option(request.baselineReservationIds).flatten.map { ids =>
      if (ids == null || ids.exists(_ <= 0) || ids.distinct.size != ids.size)
        throw new BookingRejectedException("INVALID_BOOKING_BASELINE")
      val start = try instant(request.dateTimeFrom)
      catch { case NonFatal(_) => throw new BookingRejectedException("INVALID_BOOKING_RECOVERY_TIME") }
      val end = try instant(request.dateTimeTo)
      catch { case NonFatal(_) => throw new BookingRejectedException("INVALID_BOOKING_RECOVERY_TIME") }
      if (start <= 0 || end <= start || (!request.isTelemedicine && request.clinicId <= 0) || request.serviceId <= 0 ||
          request.scheduleId <= 0 || request.doctorId <= 0)
        throw new BookingRejectedException("INVALID_BOOKING_RECOVERY_CONTEXT")
      val facts = Option(request.baselineReservations).flatten.getOrElse(
        throw new BookingRejectedException("BOOKING_BASELINE_FACTS_REQUIRED"))
      if (facts == null || facts.exists(f => f == null || f.reservationId <= 0 || f.startAt <= 0 || f.endAt <= f.startAt) ||
          facts.map(_.reservationId).distinct.size != facts.size || !facts.forall(f => ids.contains(f.reservationId)))
        throw new BookingRejectedException("INVALID_BOOKING_BASELINE_FACTS")
      (start, end, ids.sorted)
    }

  /** Compare exact reservation facts inside a complete provider coverage window.
    * A moved visit with the same ID must invalidate the bot's booking decision.
    */
  private[rest] def verifyReservationBaseline(request: BookRequest, observed: List[Event],
                                              fromAt: Long, toAt: Long): Either[Throwable, Unit] = {
    def normalized(value: Option[String]): Option[String] =
      value.flatMap(v => Option(v).map(_.trim.replaceAll("\\s+", " ").toLowerCase(java.util.Locale.ROOT)))
        .filter(_.nonEmpty)
    def inside(startAt: Long): Boolean = startAt >= fromAt && startAt < toAt
    def canonical(f: ReservationBaselineFact): ReservationBaselineFact =
      f.copy(clinicAddress = normalized(f.clinicAddress), clinicCity = normalized(f.clinicCity))
    def complete(f: ReservationBaselineFact): Boolean =
      f.reservationId > 0 && f.startAt > 0 && f.endAt > f.startAt &&
        f.clinicId.forall(_ > 0) &&
        (f.telemedicine || f.clinicId.exists(_ > 0) ||
          (normalized(f.clinicAddress).nonEmpty && normalized(f.clinicCity).nonEmpty))
    try {
      val baseline = request.baselineReservations.getOrElse(
        throw new BookingRejectedException("BOOKING_BASELINE_FACTS_REQUIRED"))
        .filter(f => inside(f.startAt)).map(canonical)
      if (baseline.exists(f => !complete(f)) || baseline.map(_.reservationId).distinct.size != baseline.size)
        throw new BookingRejectedException("INCOMPLETE_RESERVATION_BASELINE")
      val source = Option(observed).getOrElse(throw new BookingRejectedException("RESERVATION_FEED_UNVERIFIED"))
      if (source.exists(e => e == null || e.date == null))
        throw new BookingRejectedException("INCOMPLETE_RESERVATION_FEED")
      val live = source.filter(e => inside(e.date.toInstant.toEpochMilli)).map { event =>
          val end = event.dateTo.flatMap(Option(_)).getOrElse(
            throw new BookingRejectedException("INCOMPLETE_RESERVATION_FEED"))
          val telemedicine = event.eventType match {
            case Some("Telemedicine") => true
            case Some("Visit") => false
            case _ => throw new BookingRejectedException("INCOMPLETE_RESERVATION_FEED")
          }
          val clinic = event.clinic.flatMap(Option(_))
          val fact = canonical(ReservationBaselineFact(event.eventId, event.date.toInstant.toEpochMilli,
            end.toInstant.toEpochMilli, clinic.flatMap(_.id), telemedicine,
            clinic.flatMap(c => Option(c.address)), clinic.flatMap(c => Option(c.city))))
          if (!complete(fact)) throw new BookingRejectedException("INCOMPLETE_RESERVATION_FEED")
          fact
        }
      if (live.map(_.reservationId).distinct.size != live.size ||
          live.sortBy(_.reservationId) != baseline.sortBy(_.reservationId))
        Left(new BookingRejectedException("RESERVATION_SNAPSHOT_CHANGED"))
      else Right(())
    } catch { case ex: BookingRejectedException => Left(ex) }
  }

  def validateLockterm(result: ReservationLocktermResponse, allowChange: Boolean): Either[Throwable, Unit] = {
    if (result.hasErrors || result.value == null || result.value.temporaryReservationId <= 0)
      Left(new BookingRejectedException("BOOKING_REJECTED"))
    else if (!Option(result.value.valuations).exists(_.headOption.exists(_ != null)))
      Left(new BookingRejectedException("INCOMPLETE_LOCKTERM"))
    else if ({
      val valuation = result.value.valuations.head
      !valuation.price.contains(0.0) || valuation.alternativePrice.exists(_ > 0.0)
        || valuation.isReferralRequired || valuation.requireReferralForPP
    })
      Left(new BookingRejectedException("PAYMENT_OR_REFERRAL_REVIEW"))
    else if (result.value.changeTermAvailable && !allowChange)
      Left(new BookingRejectedException("ALREADY_RESERVED"))
    else if (result.value.changeTermAvailable && !Option(result.value.relatedVisits).exists(_.headOption.exists(_ != null)))
      Left(new BookingRejectedException("INCOMPLETE_LOCKTERM"))
    else Right(())
  }
}

/** The account lock survives a lost connection or process restart. Unknown means no retry. */
@Service
class BookingAttemptService(jdbc: JdbcTemplate, transactionManager: PlatformTransactionManager,
                            cancellationReceipts: CancellationReceiptService) {
  private val transaction = new TransactionTemplate(transactionManager)
  private val processId = UUID.randomUUID().toString
  private val mapper: RowMapper[BookingOutcome] = (rs, _) =>
    BookingOutcome(rs.getString("state"), Option(rs.getObject("reservation_id")).map(_.asInstanceOf[Number].longValue), Option(rs.getString("error_code")))

  def status(accountId: Long, id: String): Option[BookingOutcome] =
    jdbc.query("SELECT * FROM booking_attempt WHERE account_id=? AND id=?", mapper, Long.box(accountId), id).asScala.headOption

  def reviewContext(accountId: Long, id: String): Option[BookingRecoveryContext] =
    jdbc.query("SELECT id,state,phase,fingerprint,start_at,end_at,clinic_id,service_id,schedule_id,doctor_id,telemedicine,baseline_reservation_ids " +
      "FROM booking_attempt WHERE account_id=? AND id=?", (rs, _) => {
      val baseline = Option(rs.getString("baseline_reservation_ids"))
      val required = List("start_at", "end_at", "clinic_id", "service_id", "schedule_id", "doctor_id", "telemedicine")
      if (baseline.isEmpty || required.exists(name => rs.getObject(name) == null)) None
      else Some(BookingRecoveryContext(rs.getString("id"), rs.getString("state"), Option(rs.getString("phase")),
        rs.getString("fingerprint"), rs.getLong("start_at"), rs.getLong("end_at"), rs.getLong("clinic_id"),
        rs.getLong("service_id"), rs.getLong("schedule_id"), rs.getLong("doctor_id"),
        rs.getBoolean("telemedicine"), baseline.get.split(",").toList.filter(_.nonEmpty).map(_.toLong)))
    }, Long.box(accountId), id).asScala.headOption.flatten

  private def fingerprintForExisting(accountId: Long, id: String, request: BookRequest): String =
    if (reviewContext(accountId, id).isEmpty) BookingAttemptService.legacyFingerprint(request)
    else if (jdbc.queryForObject("SELECT baseline_reservation_facts FROM booking_attempt WHERE account_id=? AND id=?",
        classOf[String], Long.box(accountId), id) == null) BookingAttemptService.v3Fingerprint(request)
    else BookingAttemptService.fingerprint(request.copy(attemptId = None).toString)

  def accountBusy(accountId: Long): Boolean =
    cancellationReceipts.hasPending(accountId) || unresolvedRows(accountId) ||
      jdbc.queryForObject("SELECT COUNT(*) FROM booking_account_lock WHERE account_id=?", classOf[java.lang.Long], Long.box(accountId)) > 0

  /** Older image versions may have unresolved rows without an account-lock row. */
  private def unresolvedRows(accountId: Long): Boolean =
    jdbc.queryForObject("SELECT COUNT(*) FROM booking_attempt WHERE account_id=? AND " +
      "(state IN ('pending','unknown') OR (state='succeeded' AND acknowledged_at IS NULL))",
      classOf[java.lang.Long], Long.box(accountId)) > 0 ||
    jdbc.queryForObject("SELECT COUNT(*) FROM legacy_booking_barrier WHERE account_id=? AND state IN ('pending','succeeded')",
      classOf[java.lang.Long], Long.box(accountId)) > 0

  def cancellationPending(accountId: Long): Boolean = cancellationReceipts.hasPending(accountId)

  /** A previous process could not have confirmed a prepared attempt. A live process
    * that loses this race must fail its conditional phase update before confirming.
    * Rows from older images have no phase and remain locked.
    */
  def recoverPreparedAfterRestart(): Int = {
    var recovered = 0
    transaction.executeWithoutResult { _ =>
      val smart = jdbc.query(
        "SELECT id,account_id FROM booking_attempt WHERE state='pending' AND phase='prepared' AND process_id IS NOT NULL AND process_id<>?",
        (rs, _) => (rs.getString("id"), rs.getLong("account_id")), processId).asScala
      smart.foreach { case (id, accountId) =>
        val changed = jdbc.update(
          "UPDATE booking_attempt SET state='failed',error_code='PREPARED_INTERRUPTED' WHERE id=? AND account_id=? AND state='pending' AND phase='prepared' AND process_id<>?",
          id, Long.box(accountId), processId)
        if (changed == 1) {
          jdbc.update("DELETE FROM booking_account_lock WHERE account_id=? AND attempt_id=?", Long.box(accountId), id)
          recovered += 1
        }
      }
      val legacy = jdbc.query(
        "SELECT id,account_id FROM legacy_booking_barrier WHERE state='pending' AND phase='prepared' AND process_id IS NOT NULL AND process_id<>?",
        (rs, _) => (rs.getString("id"), rs.getLong("account_id")), processId).asScala
      legacy.foreach { case (id, accountId) =>
        val removed = jdbc.update(
          "DELETE FROM legacy_booking_barrier WHERE id=? AND account_id=? AND state='pending' AND phase='prepared' AND process_id<>?",
          id, Long.box(accountId), processId)
        if (removed == 1) {
          jdbc.update("DELETE FROM booking_account_lock WHERE account_id=? AND attempt_id=?", Long.box(accountId), id)
          recovered += 1
        }
      }
      // A previous process may have reached the provider and died before writing
      // its final status. Keep the account lock and make positive review possible.
      jdbc.update("UPDATE booking_attempt SET state='unknown',error_code='VERIFY_RESERVATION' " +
        "WHERE state='pending' AND phase='confirmation_started' AND process_id IS NOT NULL AND process_id<>?",
        processId)
    }
    recovered
  }

  @PostConstruct
  private def recoverOnStartup(): Unit = {
    recoverPreparedAfterRestart()
  }

  private def markConfirmationStarted(table: String, accountId: Long, id: String): Either[Throwable, Unit] = {
    try {
      val changed = jdbc.update(
        s"UPDATE $table SET phase='confirmation_started' WHERE account_id=? AND id=? AND state='pending' AND phase='prepared' AND process_id=?",
        Long.box(accountId), id, processId)
      if (changed == 1) Right(())
      else Left(new BookingNotSubmittedException(new IllegalStateException("Prepared booking is no longer owned by this process")))
    } catch { case NonFatal(error) => Left(new BookingNotSubmittedException(error)) }
  }

  def markSmartConfirmationStarted(accountId: Long, id: String): Either[Throwable, Unit] =
    markConfirmationStarted("booking_attempt", accountId, id)

  def markLegacyConfirmationStarted(accountId: Long, id: String): Either[Throwable, Unit] =
    markConfirmationStarted("legacy_booking_barrier", accountId, id)

  /** A provider confirmation must name the attempt that owns this process's durable account permit. */
  def ownsConfirmationPermit(accountId: Long, id: String): Boolean =
    id != null && jdbc.queryForObject(
      "SELECT COUNT(*) FROM booking_account_lock l WHERE l.account_id=? AND l.attempt_id=? AND (" +
        "EXISTS (SELECT 1 FROM booking_attempt a WHERE a.account_id=l.account_id AND a.id=l.attempt_id " +
        "AND a.state='pending' AND a.phase='confirmation_started' AND a.process_id=?) OR " +
        "EXISTS (SELECT 1 FROM legacy_booking_barrier b WHERE b.account_id=l.account_id AND b.id=l.attempt_id " +
        "AND b.state='pending' AND b.phase='confirmation_started' AND b.process_id=?))",
      classOf[java.lang.Long], Long.box(accountId), id, processId, processId) == 1L

  /** Once a smart policy is activated, old bot images may not use the unchecked book path. */
  /** Persist enrollment and stop all automatic monitors in one database transaction. */
  def enrollSmartBooking(accountId: Long, expectedAutoMonitorIds: Seq[Long] = Nil): Seq[Long] = {
    if (expectedAutoMonitorIds == null || expectedAutoMonitorIds.exists(_ <= 0)
        || expectedAutoMonitorIds != expectedAutoMonitorIds.distinct.sorted)
      throw new BookingRejectedException("INVALID_MONITOR_SET")
    var stopped = Seq.empty[Long]
    try transaction.executeWithoutResult { _ =>
      val permit = UUID.randomUUID().toString
      jdbc.update("INSERT INTO booking_account_lock(account_id,attempt_id) VALUES (?,?)", Long.box(accountId), permit)
      if (unresolvedRows(accountId) || cancellationReceipts.hasPending(accountId))
        throw new BookingRejectedException("ACCOUNT_BUSY")
      val alreadyEnrolled = smartBookingEnrolled(accountId)
      if (alreadyEnrolled && !smartBookingIdentitySafe(accountId))
        throw new BookingRejectedException("SMART_BOOKING_IDENTITY_UNSAFE")
      val identity = identityForAccount(accountId).getOrElse(throw new BookingRejectedException("LUXMED_IDENTITY_UNVERIFIED"))
      if (identityAccountCount(identity) != 1)
        throw new BookingRejectedException("DUPLICATE_LUXMED_IDENTITY")
      stopped = jdbc.queryForList(
        "SELECT record_id FROM monitoring WHERE account_id=? AND active=TRUE AND autobook=TRUE ORDER BY record_id",
        classOf[java.lang.Long], Long.box(accountId)).asScala.map(_.longValue()).toSeq
      if (stopped != expectedAutoMonitorIds)
        throw new BookingRejectedException("SMART_MONITORS_CHANGED")
      if (!alreadyEnrolled)
        jdbc.update("INSERT INTO smart_booking_enrollment(account_id,identity_key,enrolled_at) VALUES (?,?,?)",
          Long.box(accountId), identity, Long.box(System.currentTimeMillis()))
      jdbc.update("UPDATE monitoring SET active=FALSE WHERE account_id=? AND active=TRUE AND autobook=TRUE",
        Long.box(accountId))
      jdbc.update("DELETE FROM booking_account_lock WHERE account_id=? AND attempt_id=?", Long.box(accountId), permit)
    }
    catch {
      case _: DuplicateKeyException if accountBusy(accountId) => throw new BookingRejectedException("ACCOUNT_BUSY")
      case _: DuplicateKeyException => throw new BookingRejectedException("DUPLICATE_LUXMED_IDENTITY")
    }
    stopped
  }

  def smartBookingEnrolled(accountId: Long): Boolean =
    jdbc.queryForObject("SELECT COUNT(*) FROM smart_booking_enrollment WHERE account_id=?", classOf[java.lang.Long], Long.box(accountId)) > 0

  def identityForAccount(accountId: Long): Option[String] = {
    val names = jdbc.queryForList("SELECT DISTINCT LOWER(TRIM(username)) FROM credentials WHERE account_id=?",
      classOf[String], Long.box(accountId)).asScala.filter(s => s != null && s.nonEmpty)
    if (names.size == 1) names.headOption else None
  }

  private def identityAccountCount(identity: String): Long =
    jdbc.queryForObject("SELECT COUNT(DISTINCT account_id) FROM credentials WHERE LOWER(TRIM(username))=?",
      classOf[java.lang.Long], identity).longValue()

  def smartBookingIdentitySafe(accountId: Long): Boolean =
    identityForAccount(accountId).exists { identity =>
      identityAccountCount(identity) == 1 &&
        jdbc.queryForObject("SELECT COUNT(*) FROM smart_booking_enrollment WHERE account_id=? AND identity_key=?",
          classOf[java.lang.Long], Long.box(accountId), identity) == 1
    }

  def enrolledAccountForUsername(username: String): Option[Long] = {
    val normalized = Option(username).map(_.trim.toLowerCase(java.util.Locale.ROOT)).getOrElse("")
    if (normalized.isEmpty) None
    else jdbc.queryForList("SELECT account_id FROM smart_booking_enrollment WHERE identity_key=?",
      classOf[java.lang.Long], normalized).asScala.headOption.map(_.longValue())
  }

  def identityHasSmartEnrollment(accountId: Long): Boolean =
    smartBookingEnrolled(accountId) ||
      jdbc.queryForObject(
        "SELECT COUNT(*) FROM credentials c JOIN smart_booking_enrollment e ON LOWER(TRIM(c.username))=e.identity_key WHERE c.account_id=?",
        classOf[java.lang.Long], Long.box(accountId)) > 0

  /** Serialize creation of an automatic legacy monitor with smart enrollment. */
  def withAutoMonitorWrite[A](accountId: Long)(body: => A): A = {
    var result: A = null.asInstanceOf[A]
    try transaction.executeWithoutResult { _ =>
      val permit = UUID.randomUUID().toString
      jdbc.update("INSERT INTO booking_account_lock(account_id,attempt_id) VALUES (?,?)", Long.box(accountId), permit)
      if (identityHasSmartEnrollment(accountId))
        throw new BookingRejectedException("SMART_BOOKING_ENROLLED")
      if (unresolvedRows(accountId) || cancellationReceipts.hasPending(accountId))
        throw new BookingRejectedException("ACCOUNT_BUSY")
      result = body
      jdbc.update("DELETE FROM booking_account_lock WHERE account_id=? AND attempt_id=?", Long.box(accountId), permit)
    } catch { case _: DuplicateKeyException => throw new BookingRejectedException("ACCOUNT_BUSY") }
    result
  }

  /** Record the legacy attempt before it can reach LuxMed. A crash leaves a safe pending barrier. */
  def beginLegacyBooking(accountId: Long, start: Long, monitoringId: Option[Long] = None): String = {
    val id = UUID.randomUUID().toString
    try transaction.executeWithoutResult { _ =>
      jdbc.update("INSERT INTO booking_account_lock(account_id,attempt_id) VALUES (?,?)", Long.box(accountId), id)
      if (identityHasSmartEnrollment(accountId))
        throw new BookingRejectedException("SMART_BOOKING_ENROLLED")
      if (cancellationReceipts.hasPending(accountId) || unresolvedRows(accountId))
        throw new BookingRejectedException("ACCOUNT_BUSY")
      jdbc.update("INSERT INTO legacy_booking_barrier(id,account_id,state,monitoring_id,start_at,created_at,phase,process_id) VALUES (?,?,'pending',?,?,?,'prepared',?)",
        id, Long.box(accountId), monitoringId.map(Long.box).orNull, Long.box(start), Long.box(System.currentTimeMillis()), processId)
    } catch { case _: DuplicateKeyException => throw new BookingRejectedException("ACCOUNT_BUSY") }
    id
  }

  /** A missing receipt blocks every legacy monitor. A known success blocks its source monitor. */
  def canLegacyMonitorBook(accountId: Long, monitoringId: Long): Boolean =
    !identityHasSmartEnrollment(accountId) && !accountBusy(accountId) &&
      jdbc.queryForObject(
      "SELECT COUNT(*) FROM legacy_booking_barrier WHERE account_id=? AND (state='pending' OR (state='succeeded' AND monitoring_id=?))",
      classOf[java.lang.Long], Long.box(accountId), Long.box(monitoringId)) == 0

  def completeLegacyBooking(accountId: Long, id: String, reservationId: Long): Unit = {
    require(reservationId > 0, "Legacy booking receipt has no reservation ID")
    val changed = jdbc.update("UPDATE legacy_booking_barrier SET state='succeeded',reservation_id=? WHERE account_id=? AND id=? AND state='pending'",
      Long.box(reservationId), Long.box(accountId), id)
    require(changed == 1, "Legacy booking barrier is missing")
  }

  /** Only failures known to precede confirmation may release a pending barrier. */
  def clearLegacyBeforeSubmission(accountId: Long, id: String): Unit = {
    transaction.executeWithoutResult { _ =>
      val removed = jdbc.update("DELETE FROM legacy_booking_barrier WHERE account_id=? AND id=? AND state='pending' AND phase='prepared' AND process_id=?",
        Long.box(accountId), id, processId)
      if (removed == 1) jdbc.update("DELETE FROM booking_account_lock WHERE account_id=? AND attempt_id=?", Long.box(accountId), id)
    }
  }

  private def ownedPreparedAttempt(accountId: Long, id: String): Boolean =
    try jdbc.queryForObject(
      "SELECT COUNT(*) FROM booking_attempt WHERE account_id=? AND id=? AND state='pending' AND phase='prepared' AND process_id=?",
      classOf[java.lang.Long], Long.box(accountId), id, processId) == 1L
    catch { case NonFatal(_) => false }

  def legacyBarrier(accountId: Long): LegacyBookingBarrier = {
    val rows = jdbc.query("SELECT id,state,reservation_id,start_at FROM legacy_booking_barrier WHERE account_id=? ORDER BY created_at,id",
      (rs, _) => LegacyBookingBarrier(rs.getString("state"),
        Option(rs.getObject("reservation_id")).map(_.asInstanceOf[Number].longValue),
        Option(rs.getObject("start_at")).map(_.asInstanceOf[Number].longValue), Some(rs.getString("id"))), Long.box(accountId)).asScala
    rows.find(_.state == "pending").orElse(rows.headOption).getOrElse(LegacyBookingBarrier("clear"))
  }

  def acknowledgeLegacyBooking(accountId: Long, id: String, reservationId: Long, startAt: Long): Boolean = {
    if (id == null || !id.matches("[a-fA-F0-9-]{36}") || reservationId <= 0 || startAt <= 0) return false
    var removed = 0
    transaction.executeWithoutResult { _ =>
      removed = jdbc.update("DELETE FROM legacy_booking_barrier WHERE account_id=? AND id=? AND reservation_id=? AND start_at=? AND state='succeeded'",
        Long.box(accountId), id, Long.box(reservationId), Long.box(startAt))
      if (removed == 1) jdbc.update("DELETE FROM booking_account_lock WHERE account_id=? AND attempt_id=?", Long.box(accountId), id)
    }
    removed == 1
  }

  /** Old bots send only the reservation ID. Accept that shape only for one succeeded
    * barrier with its matching lock, or a pre-v3 barrier with no account lock.
    */
  def acknowledgeLegacyBookingV1(accountId: Long, reservationId: Long): Boolean = {
    if (reservationId <= 0) return false
    var removed = 0
    transaction.executeWithoutResult { _ =>
      val matches = jdbc.query(
        "SELECT b.id,b.start_at,l.attempt_id FROM legacy_booking_barrier b LEFT JOIN booking_account_lock l " +
          "ON l.account_id=b.account_id " +
          "WHERE b.account_id=? AND b.reservation_id=? AND b.state='succeeded'",
        (rs, _) => (rs.getString("id"), rs.getLong("start_at"), Option(rs.getString("attempt_id"))),
        Long.box(accountId), Long.box(reservationId)).asScala
      if (matches.size == 1 && matches.head._3.forall(_ == matches.head._1)) {
        val (id, startAt, _) = matches.head
        removed = jdbc.update("DELETE FROM legacy_booking_barrier WHERE account_id=? AND id=? AND reservation_id=? AND start_at=? AND state='succeeded'",
          Long.box(accountId), id, Long.box(reservationId), Long.box(startAt))
        if (removed == 1) jdbc.update("DELETE FROM booking_account_lock WHERE account_id=? AND attempt_id=?", Long.box(accountId), id)
      }
    }
    removed == 1
  }

  def beginCancellation(accountId: Long, reservationId: Long, startAt: Long): Unit = {
    try transaction.executeWithoutResult { _ =>
      jdbc.update("INSERT INTO booking_account_lock(account_id,attempt_id) VALUES (?,?)",
        Long.box(accountId), s"cancel:$reservationId")
      if (unresolvedRows(accountId) || cancellationReceipts.hasPending(accountId))
        throw new BookingRejectedException("ACCOUNT_BUSY")
      cancellationReceipts.begin(accountId, reservationId, startAt)
    } catch { case _: DuplicateKeyException => throw new BookingRejectedException("ACCOUNT_BUSY") }
  }

  def reviewCancellation(accountId: Long, reservationId: Long, startAt: Long, action: String,
                         operator: String, reason: String, providerRequestSettled: Boolean,
                         cancellationStatusVerified: Boolean,
                         expectedMovedStartAt: Option[Long],
                         observed: List[Event]): com.lbs.server.service.CancellationReceipt = {
    if (!providerRequestSettled || observed == null)
      throw new BookingRejectedException("CANCELLATION_REVIEW_EVIDENCE_MISSING")
    if (action == "confirmed_cancelled" && !cancellationStatusVerified)
      throw new BookingRejectedException("CANCELLATION_STATUS_NOT_VERIFIED")
    val matching = observed.filter(_.eventId == reservationId)
    if (matching.exists(_.date == null))
      throw new BookingRejectedException("INCOMPLETE_RESERVATION_FEED")
    if (action == "confirmed_cancelled" && matching.nonEmpty)
      throw new BookingRejectedException("RESERVATION_STILL_PRESENT")
    if (action == "verified_still_reserved" &&
        !matching.exists(_.date.toInstant.toEpochMilli == startAt))
      throw new BookingRejectedException("RESERVATION_NOT_VERIFIED")
    val moved = if (action == "verified_moved") {
      val expected = expectedMovedStartAt.getOrElse(
        throw new BookingRejectedException("MOVED_RESERVATION_START_REQUIRED"))
      if (expected <= 0 || expected == startAt || matching.size != 1 ||
          matching.head.date.toInstant.toEpochMilli != expected)
        throw new BookingRejectedException("MOVED_RESERVATION_NOT_VERIFIED")
      val event = matching.head
      val end = event.dateTo.flatMap(Option(_)).getOrElse(
        throw new BookingRejectedException("MOVED_RESERVATION_INCOMPLETE"))
      val telemedicine = event.eventType match {
        case Some("Telemedicine") => true
        case Some("Visit") => false
        case _ => throw new BookingRejectedException("MOVED_RESERVATION_INCOMPLETE")
      }
      val clinic = event.clinic.flatMap(Option(_))
      val clinicId = clinic.flatMap(_.id).filter(_ > 0)
      val address = clinic.flatMap(c => Option(c.address)).map(_.trim).filter(_.nonEmpty)
      val city = clinic.flatMap(c => Option(c.city)).map(_.trim).filter(_.nonEmpty)
      if (end.toInstant.toEpochMilli <= expected ||
          (!telemedicine && clinicId.isEmpty && (address.isEmpty || city.isEmpty)))
        throw new BookingRejectedException("MOVED_RESERVATION_INCOMPLETE")
      Some(MovedReservationFact(expected, end.toInstant.toEpochMilli, clinicId, telemedicine, address, city))
    } else {
      if (expectedMovedStartAt.nonEmpty)
        throw new BookingRejectedException("UNEXPECTED_MOVED_RESERVATION_START")
      None
    }
    var reviewed: com.lbs.server.service.CancellationReceipt = null
    transaction.executeWithoutResult { _ =>
      reviewed = cancellationReceipts.review(accountId, reservationId, startAt, action, operator, reason, moved)
      val audited = jdbc.update("UPDATE cancellation_receipt_review_audit SET provider_request_settled=TRUE," +
        "cancellation_status_verified=? " +
        "WHERE account_id=? AND reservation_id=? AND start_at=? AND reviewed_at=?",
        Boolean.box(cancellationStatusVerified), Long.box(accountId), Long.box(reservationId),
        Long.box(startAt), reviewed.reviewedAt.map(Long.box).orNull)
      if (audited != 1) throw new BookingRejectedException("CANCELLATION_REVIEW_AUDIT_FAILED")
      if (action != "verified_moved")
        jdbc.update("DELETE FROM booking_account_lock WHERE account_id=? AND attempt_id=?",
          Long.box(accountId), s"cancel:$reservationId")
    }
    reviewed
  }

  /** The bot ACKs only after saving the exact moved visit in its local snapshot. */
  def acknowledgeMovedCancellation(accountId: Long, reservationId: Long,
                                   originalStartAt: Long, movedStartAt: Long): Boolean = {
    if (accountId <= 0 || reservationId <= 0 || originalStartAt <= 0 || movedStartAt <= 0) return false
    var matched = false
    transaction.executeWithoutResult { _ =>
      val rows = jdbc.query(
        "SELECT moved_acknowledged_at FROM cancellation_receipt WHERE account_id=? AND reservation_id=? " +
          "AND start_at=? AND moved_start_at=? AND state='verified_moved'",
        (rs, _) => Option(rs.getObject("moved_acknowledged_at")).map(_.asInstanceOf[Number].longValue()),
        Long.box(accountId), Long.box(reservationId), Long.box(originalStartAt), Long.box(movedStartAt)).asScala
      if (rows.size == 1) {
        matched = true
        if (rows.head.isEmpty) {
          val now = System.currentTimeMillis()
          val changed = jdbc.update("UPDATE cancellation_receipt SET moved_acknowledged_at=? WHERE account_id=? " +
            "AND reservation_id=? AND start_at=? AND moved_start_at=? AND state='verified_moved' " +
            "AND moved_acknowledged_at IS NULL",
            Long.box(now), Long.box(accountId), Long.box(reservationId), Long.box(originalStartAt), Long.box(movedStartAt))
          if (changed != 1) throw new BookingRejectedException("MOVED_ACK_TARGET_CHANGED")
          val audited = jdbc.update("UPDATE cancellation_receipt_review_audit SET moved_acknowledged_at=? " +
            "WHERE account_id=? AND reservation_id=? AND start_at=? AND moved_start_at=? " +
            "AND new_state='verified_moved' AND moved_acknowledged_at IS NULL",
            Long.box(now), Long.box(accountId), Long.box(reservationId), Long.box(originalStartAt), Long.box(movedStartAt))
          if (audited != 1) throw new BookingRejectedException("MOVED_ACK_AUDIT_FAILED")
          val unlocked = jdbc.update("DELETE FROM booking_account_lock WHERE account_id=? AND attempt_id=?",
            Long.box(accountId), s"cancel:$reservationId")
          if (unlocked != 1) throw new BookingRejectedException("MOVED_ACK_LOCK_MISSING")
        }
      }
    }
    matched
  }

  def acknowledgeSmartBooking(accountId: Long, id: String, reservationId: Long): Boolean = {
    if (id == null || !id.matches("[a-fA-F0-9-]{36}") || reservationId <= 0) return false
    var matched = false
    transaction.executeWithoutResult { _ =>
      matched = jdbc.queryForObject(
        "SELECT COUNT(*) FROM booking_attempt WHERE account_id=? AND id=? AND state='succeeded' AND reservation_id=?",
        classOf[java.lang.Long], Long.box(accountId), id, Long.box(reservationId)) == 1L
      if (matched) {
        jdbc.update("UPDATE booking_attempt SET acknowledged_at=COALESCE(acknowledged_at,?) " +
          "WHERE account_id=? AND id=? AND state='succeeded' AND reservation_id=?",
          Long.box(System.currentTimeMillis()), Long.box(accountId), id, Long.box(reservationId))
        jdbc.update("DELETE FROM booking_account_lock WHERE account_id=? AND attempt_id=?", Long.box(accountId), id)
      }
    }
    matched
  }

  /** Only a positive, exact reservation match can resolve an unknown confirmation.
    * The operator verifies service and doctor in the portal because the event feed
    * does not expose those identifiers. No absent-feed result releases the lock.
    */
  def reviewPositiveAttempt(accountId: Long, id: String, request: BookingPositiveReviewRequest,
                            observed: List[Event]): BookingOutcome = {
    val context = reviewContext(accountId, id).getOrElse(
      throw new BookingRejectedException("BOOKING_REVIEW_CONTEXT_MISSING"))
    val operator = Option(request).flatMap(r => Option(r.operator)).map(_.trim).getOrElse("")
    val reason = Option(request).flatMap(r => Option(r.reason)).map(_.trim).getOrElse("")
    val baseline = Option(request).flatMap(r => Option(r.expectedBaselineReservationIds))
    if (context.state != "unknown" || context.phase != Some("confirmation_started") ||
        request == null || request.reservationId <= 0 ||
        request.expectedFingerprint != context.fingerprint ||
        request.expectedStartAt != context.startAt || request.expectedEndAt != context.endAt ||
        request.expectedClinicId != context.clinicId ||
        baseline.isEmpty || baseline.get != context.baselineReservationIds ||
        context.baselineReservationIds.contains(request.reservationId) ||
        !request.serviceAndDoctorVerified || !request.providerRequestSettled ||
        operator.isEmpty || operator.length > 128 || reason.isEmpty || reason.length > 1000)
      throw new BookingRejectedException("BOOKING_REVIEW_TARGET_CHANGED")
    val matches = Option(observed).getOrElse(Nil).count { event =>
      event.status == "Reserved" && event.eventId == request.reservationId &&
        event.date.toInstant.toEpochMilli == context.startAt &&
        event.dateTo.exists(_.toInstant.toEpochMilli == context.endAt) &&
        (if (context.telemedicine) event.eventType.contains("Telemedicine")
         else event.eventType.contains("Visit") && event.clinic.flatMap(_.id).contains(context.clinicId))
    }
    if (matches != 1) throw new BookingRejectedException("BOOKING_REVIEW_RESERVATION_NOT_VERIFIED")
    var changed = 0
    transaction.executeWithoutResult { _ =>
      val lock = jdbc.queryForObject(
        "SELECT COUNT(*) FROM booking_account_lock WHERE account_id=? AND attempt_id=?",
        classOf[java.lang.Long], Long.box(accountId), id)
      if (lock != 1L) throw new BookingRejectedException("BOOKING_REVIEW_LOCK_CHANGED")
      changed = jdbc.update(
        "UPDATE booking_attempt SET state='succeeded',reservation_id=?,error_code=NULL WHERE account_id=? AND id=? " +
          "AND state='unknown' AND phase='confirmation_started' AND fingerprint=? AND start_at=? AND end_at=? AND clinic_id=?",
        Long.box(request.reservationId), Long.box(accountId), id, context.fingerprint,
        Long.box(context.startAt), Long.box(context.endAt), Long.box(context.clinicId))
      if (changed != 1) throw new BookingRejectedException("BOOKING_REVIEW_TARGET_CHANGED")
      jdbc.update(
        "INSERT INTO booking_attempt_review_audit(account_id,attempt_id,fingerprint,reservation_id,start_at,end_at,clinic_id," +
        "service_id,schedule_id,doctor_id,telemedicine,baseline_reservation_ids,service_and_doctor_verified," +
          "provider_request_settled,reviewed_by,reason,reviewed_at) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)",
        Long.box(accountId), id, context.fingerprint, Long.box(request.reservationId),
        Long.box(context.startAt), Long.box(context.endAt), Long.box(context.clinicId),
        Long.box(context.serviceId), Long.box(context.scheduleId), Long.box(context.doctorId),
        Boolean.box(context.telemedicine),
        context.baselineReservationIds.mkString(","), Boolean.box(true), Boolean.box(true),
        operator, reason, Long.box(System.currentTimeMillis()))
    }
    BookingOutcome("succeeded", Some(request.reservationId))
  }

  def execute(accountId: Long, request: BookRequest)(book: () => Either[Throwable, ReservationConfirmResponse]): BookingOutcome = {
    val id = request.attemptId.get
    require(id.matches("[a-fA-F0-9-]{36}"), "Invalid booking attempt ID")
    val existing = status(accountId, id)
    if (existing.nonEmpty) {
      val old = jdbc.queryForObject("SELECT fingerprint FROM booking_attempt WHERE account_id=? AND id=?", classOf[String], Long.box(accountId), id)
      val fingerprint = fingerprintForExisting(accountId, id, request)
      require(old == fingerprint, "Booking attempt ID already has another payload")
      return existing.get
    }
    val recovery = try BookingAttemptService.recoveryFacts(request)
    catch { case ex: BookingRejectedException => return BookingOutcome("failed", errorCode = Some(ex.code)) }
    if (recovery.isEmpty) return BookingOutcome("failed", errorCode = Some("BOOKING_BASELINE_REQUIRED"))
    val fingerprint = BookingAttemptService.fingerprint(request.copy(attemptId = None).toString)
    try {
      transaction.executeWithoutResult { _ =>
        jdbc.update("INSERT INTO booking_account_lock(account_id,attempt_id) VALUES (?,?)", Long.box(accountId), id)
        if (unresolvedRows(accountId) || cancellationReceipts.hasPending(accountId))
          throw new DuplicateKeyException("ACCOUNT_BUSY")
        jdbc.update("INSERT INTO booking_attempt(id,account_id,fingerprint,state,created_at,phase,process_id," +
          "start_at,end_at,clinic_id,service_id,schedule_id,doctor_id,telemedicine,baseline_reservation_ids,baseline_reservation_facts) " +
          "VALUES (?,?,?,'pending',?,'prepared',?,?,?,?,?,?,?,?,?,?)",
          id, Long.box(accountId), fingerprint, Long.box(System.currentTimeMillis()), processId,
          recovery.map(f => Long.box(f._1)).orNull, recovery.map(f => Long.box(f._2)).orNull,
          recovery.map(_ => Long.box(request.clinicId)).orNull,
          recovery.map(_ => Long.box(request.serviceId)).orNull,
          recovery.map(_ => Long.box(request.scheduleId)).orNull,
          recovery.map(_ => Long.box(request.doctorId)).orNull,
          recovery.map(_ => Boolean.box(request.isTelemedicine)).orNull,
          recovery.map(_._3.mkString(",")).orNull,
          request.baselineReservations.map(_.sortBy(_.reservationId).mkString("|")).orNull)
      }
    } catch {
      case _: DuplicateKeyException =>
        val raced = status(accountId, id)
        if (raced.nonEmpty) {
          val old = jdbc.queryForObject("SELECT fingerprint FROM booking_attempt WHERE account_id=? AND id=?", classOf[String], Long.box(accountId), id)
          require(old == fingerprintForExisting(accountId, id, request), "Booking attempt ID already has another payload")
          return raced.get
        }
        try transaction.executeWithoutResult { _ =>
          jdbc.update("INSERT INTO booking_attempt(id,account_id,fingerprint,state,error_code,created_at) VALUES (?,?,?,'failed','ACCOUNT_BUSY',?)",
            id, Long.box(accountId), fingerprint, Long.box(System.currentTimeMillis()))
        } catch {
          case _: DuplicateKeyException =>
            val old = jdbc.queryForObject("SELECT fingerprint FROM booking_attempt WHERE account_id=? AND id=?", classOf[String], Long.box(accountId), id)
            require(old == fingerprintForExisting(accountId, id, request), "Booking attempt ID already has another payload")
        }
        return status(accountId, id).getOrElse(BookingOutcome("unknown", errorCode = Some("VERIFY_RESERVATION")))
    }
    val outcome = try {
      book() match {
        case Right(result) if !result.hasErrors && result.value != null && result.value.reservationId > 0 =>
          BookingOutcome("succeeded", Some(result.value.reservationId))
        case Left(error: BookingRejectedException) if ownedPreparedAttempt(accountId, id) =>
          BookingOutcome("failed", errorCode = Some(error.code))
        case Left(_: BookingNotSubmittedException) if ownedPreparedAttempt(accountId, id) =>
          BookingOutcome("failed", errorCode = Some("BOOKING_NOT_SUBMITTED"))
        case _ => BookingOutcome("unknown", errorCode = Some("VERIFY_RESERVATION"))
      }
    } catch {
      case NonFatal(_) if ownedPreparedAttempt(accountId, id) =>
        BookingOutcome("failed", errorCode = Some("BOOKING_NOT_SUBMITTED"))
      case NonFatal(_) => BookingOutcome("unknown", errorCode = Some("VERIFY_RESERVATION"))
    }
    var changed = 0
    transaction.executeWithoutResult { _ =>
      changed = jdbc.update("UPDATE booking_attempt SET state=?,reservation_id=?,error_code=? WHERE account_id=? AND id=? AND state='pending' AND process_id=?",
        outcome.state, outcome.reservationId.map(Long.box).orNull, outcome.errorCode.orNull, Long.box(accountId), id, processId)
      if (changed == 1 && outcome.state == "failed")
        jdbc.update("DELETE FROM booking_account_lock WHERE account_id=? AND attempt_id=?", Long.box(accountId), id)
    }
    if (changed == 1) outcome else status(accountId, id).getOrElse(BookingOutcome("unknown", errorCode = Some("VERIFY_RESERVATION")))
  }
}
