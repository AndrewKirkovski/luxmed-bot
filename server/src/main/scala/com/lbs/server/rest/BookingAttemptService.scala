package com.lbs.server.rest

import com.lbs.api.json.model.{ReservationConfirmResponse, ReservationLocktermResponse}
import org.springframework.dao.DuplicateKeyException
import org.springframework.jdbc.core.{JdbcTemplate, RowMapper}
import org.springframework.stereotype.Service
import org.springframework.transaction.PlatformTransactionManager
import org.springframework.transaction.support.TransactionTemplate
import com.lbs.server.service.CancellationReceiptService
import jakarta.annotation.PostConstruct
import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest
import java.util.UUID
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

class BookingNotSubmittedException(cause: Throwable) extends RuntimeException("Booking was not submitted", cause)
class BookingRejectedException(val code: String) extends RuntimeException(code)
case class LegacyBookingBarrier(state: String, reservationId: Option[Long] = None, start: Option[Long] = None)
case class LegacyBookingAcknowledgeRequest(reservationId: Long)
case class SmartBookingEnrollment(enrolled: Boolean, stoppedAutoMonitorIds: Seq[Long] = Nil)
case class SmartBookingEnrollmentRequest(expectedAutoMonitorIds: List[Long])

object BookingAttemptService {
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

  def accountBusy(accountId: Long): Boolean =
    cancellationReceipts.hasPending(accountId) ||
      jdbc.queryForObject("SELECT COUNT(*) FROM booking_account_lock WHERE account_id=?", classOf[java.lang.Long], Long.box(accountId)) > 0

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
        recovered += jdbc.update(
          "DELETE FROM legacy_booking_barrier WHERE id=? AND account_id=? AND state='pending' AND phase='prepared' AND process_id<>?",
          id, Long.box(accountId), processId)
      }
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

  /** Once a smart policy is activated, old bot images may not use the unchecked book path. */
  /** Persist enrollment and stop all automatic monitors in one database transaction. */
  def enrollSmartBooking(accountId: Long, expectedAutoMonitorIds: Seq[Long] = Nil): Seq[Long] = {
    if (expectedAutoMonitorIds == null || expectedAutoMonitorIds.exists(_ <= 0)
        || expectedAutoMonitorIds != expectedAutoMonitorIds.distinct.sorted)
      throw new BookingRejectedException("INVALID_MONITOR_SET")
    val alreadyEnrolled = smartBookingEnrolled(accountId)
    if (alreadyEnrolled && !smartBookingIdentitySafe(accountId))
      throw new BookingRejectedException("SMART_BOOKING_IDENTITY_UNSAFE")
    val identity = identityForAccount(accountId).getOrElse(throw new BookingRejectedException("LUXMED_IDENTITY_UNVERIFIED"))
    if (identityAccountCount(identity) != 1)
      throw new BookingRejectedException("DUPLICATE_LUXMED_IDENTITY")
    var stopped = Seq.empty[Long]
    try transaction.executeWithoutResult { _ =>
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
    }
    catch {
      case _: DuplicateKeyException if smartBookingIdentitySafe(accountId) => ()
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

  /** Record the legacy attempt before it can reach LuxMed. A crash leaves a safe pending barrier. */
  def beginLegacyBooking(accountId: Long, start: Long, monitoringId: Option[Long] = None): String = {
    val id = UUID.randomUUID().toString
    jdbc.update("INSERT INTO legacy_booking_barrier(id,account_id,state,monitoring_id,start_at,created_at,phase,process_id) VALUES (?,?,'pending',?,?,?,'prepared',?)",
      id, Long.box(accountId), monitoringId.map(Long.box).orNull, Long.box(start), Long.box(System.currentTimeMillis()), processId)
    id
  }

  /** A missing receipt blocks every legacy monitor. A known success blocks its source monitor. */
  def canLegacyMonitorBook(accountId: Long, monitoringId: Long): Boolean =
    !identityHasSmartEnrollment(accountId) && !cancellationReceipts.hasPending(accountId) &&
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
    jdbc.update("DELETE FROM legacy_booking_barrier WHERE account_id=? AND id=? AND state='pending' AND phase='prepared' AND process_id=?",
      Long.box(accountId), id, processId)
  }

  private def ownedPreparedAttempt(accountId: Long, id: String): Boolean =
    try jdbc.queryForObject(
      "SELECT COUNT(*) FROM booking_attempt WHERE account_id=? AND id=? AND state='pending' AND phase='prepared' AND process_id=?",
      classOf[java.lang.Long], Long.box(accountId), id, processId) == 1L
    catch { case NonFatal(_) => false }

  def legacyBarrier(accountId: Long): LegacyBookingBarrier = {
    val rows = jdbc.query("SELECT state,reservation_id,start_at FROM legacy_booking_barrier WHERE account_id=? ORDER BY created_at,id",
      (rs, _) => LegacyBookingBarrier(rs.getString("state"),
        Option(rs.getObject("reservation_id")).map(_.asInstanceOf[Number].longValue),
        Option(rs.getObject("start_at")).map(_.asInstanceOf[Number].longValue)), Long.box(accountId)).asScala
    rows.find(_.state == "pending").orElse(rows.headOption).getOrElse(LegacyBookingBarrier("clear"))
  }

  def acknowledgeLegacyBooking(accountId: Long, reservationId: Long): Boolean = {
    if (reservationId <= 0) return false
    jdbc.update("DELETE FROM legacy_booking_barrier WHERE account_id=? AND reservation_id=? AND state='succeeded'",
      Long.box(accountId), Long.box(reservationId)) > 0
  }

  def execute(accountId: Long, request: BookRequest)(book: () => Either[Throwable, ReservationConfirmResponse]): BookingOutcome = {
    val id = request.attemptId.get
    require(id.matches("[a-fA-F0-9-]{36}"), "Invalid booking attempt ID")
    val fingerprint = MessageDigest.getInstance("SHA-256").digest(request.copy(attemptId = None).toString.getBytes(UTF_8)).map(b => f"${b & 0xff}%02x").mkString
    val existing = status(accountId, id)
    if (existing.nonEmpty) {
      val old = jdbc.queryForObject("SELECT fingerprint FROM booking_attempt WHERE account_id=? AND id=?", classOf[String], Long.box(accountId), id)
      require(old == fingerprint, "Booking attempt ID already has another payload")
      return existing.get
    }
    try {
      transaction.executeWithoutResult { _ =>
        jdbc.update("INSERT INTO booking_account_lock(account_id,attempt_id) VALUES (?,?)", Long.box(accountId), id)
        jdbc.update("INSERT INTO booking_attempt(id,account_id,fingerprint,state,created_at,phase,process_id) VALUES (?,?,?,'pending',?,'prepared',?)",
          id, Long.box(accountId), fingerprint, Long.box(System.currentTimeMillis()), processId)
      }
    } catch {
      case _: DuplicateKeyException =>
        val raced = status(accountId, id)
        if (raced.nonEmpty) {
          val old = jdbc.queryForObject("SELECT fingerprint FROM booking_attempt WHERE account_id=? AND id=?", classOf[String], Long.box(accountId), id)
          require(old == fingerprint, "Booking attempt ID already has another payload")
          return raced.get
        }
        try transaction.executeWithoutResult { _ =>
          jdbc.update("INSERT INTO booking_attempt(id,account_id,fingerprint,state,error_code,created_at) VALUES (?,?,?,'failed','ACCOUNT_BUSY',?)",
            id, Long.box(accountId), fingerprint, Long.box(System.currentTimeMillis()))
        } catch {
          case _: DuplicateKeyException =>
            val old = jdbc.queryForObject("SELECT fingerprint FROM booking_attempt WHERE account_id=? AND id=?", classOf[String], Long.box(accountId), id)
            require(old == fingerprint, "Booking attempt ID already has another payload")
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
      if (changed == 1 && (outcome.state == "succeeded" || outcome.state == "failed"))
        jdbc.update("DELETE FROM booking_account_lock WHERE account_id=? AND attempt_id=?", Long.box(accountId), id)
    }
    if (changed == 1) outcome else status(accountId, id).getOrElse(BookingOutcome("unknown", errorCode = Some("VERIFY_RESERVATION")))
  }
}
