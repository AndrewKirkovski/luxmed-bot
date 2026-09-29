package com.lbs.server.service

import com.lbs.server.rest.BookingRejectedException
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.dao.DuplicateKeyException
import org.springframework.jdbc.core.JdbcTemplate
import org.springframework.jdbc.datasource.DataSourceTransactionManager
import org.springframework.stereotype.Service
import org.springframework.transaction.PlatformTransactionManager
import org.springframework.transaction.support.TransactionTemplate

import scala.jdk.CollectionConverters.*

case class CancellationReceipt(accountId: Long, reservationId: Long, startAt: Long,
                               state: String, confirmedAt: Option[Long] = None,
                               reviewedAt: Option[Long] = None, reviewedBy: Option[String] = None,
                               reviewReason: Option[String] = None, reviewAction: Option[String] = None,
                               movedStartAt: Option[Long] = None, movedEndAt: Option[Long] = None,
                               movedClinicId: Option[Long] = None, movedTelemedicine: Option[Boolean] = None,
                               movedClinicAddress: Option[String] = None, movedClinicCity: Option[String] = None,
                               acknowledgedAt: Option[Long] = None)

case class MovedReservationFact(startAt: Long, endAt: Long, clinicId: Option[Long],
                                telemedicine: Boolean, clinicAddress: Option[String], clinicCity: Option[String])

/** Records intent before DELETE. Only an explicit operator review can resolve it. */
@Service
class CancellationReceiptService @Autowired()(jdbc: JdbcTemplate, transactionManager: PlatformTransactionManager) {
  def this(jdbc: JdbcTemplate) = this(jdbc, new DataSourceTransactionManager(jdbc.getDataSource))

  private val transaction = new TransactionTemplate(transactionManager)

  def begin(accountId: Long, reservationId: Long, startAt: Long): Unit = {
    require(accountId > 0 && reservationId > 0 && startAt > 0, "Invalid cancellation receipt")
    try jdbc.update(
      "INSERT INTO cancellation_receipt(account_id,reservation_id,start_at,state,requested_at) VALUES (?,?,?,'pending',?)",
      Long.box(accountId), Long.box(reservationId), Long.box(startAt), Long.box(System.currentTimeMillis()))
    catch {
      case _: DuplicateKeyException =>
        // A repeated HTTP request must not dispatch DELETE again, even after review.
        throw new BookingRejectedException("CANCELLATION_ATTEMPT_ALREADY_EXISTS")
    }
  }

  def review(accountId: Long, reservationId: Long, startAt: Long,
             action: String, operator: String, reason: String,
             moved: Option[MovedReservationFact] = None): CancellationReceipt = {
    val normalizedOperator = Option(operator).map(_.trim).getOrElse("")
    val normalizedReason = Option(reason).map(_.trim).getOrElse("")
    val newState = action match {
      case "confirmed_cancelled" => "confirmed"
      case "verified_still_reserved" => "verified_still_reserved"
      case "verified_moved" => "verified_moved"
      case _ => throw new BookingRejectedException("INVALID_CANCELLATION_REVIEW_ACTION")
    }
    if (accountId <= 0 || reservationId <= 0 || startAt <= 0 ||
      (action == "verified_moved" && moved.isEmpty) ||
      (action != "verified_moved" && moved.nonEmpty) || normalizedOperator.isEmpty ||
      normalizedOperator.length > 128 || normalizedReason.isEmpty || normalizedReason.length > 1000)
      throw new BookingRejectedException("INVALID_CANCELLATION_REVIEW")

    transaction.execute[CancellationReceipt](_ => {
      val reviewedAt = System.currentTimeMillis()
      val changed = jdbc.update(
        "UPDATE cancellation_receipt SET state=?,confirmed_at=?,reviewed_at=?,reviewed_by=?,review_reason=?,review_action=?," +
          "moved_start_at=?,moved_end_at=?,moved_clinic_id=?,moved_telemedicine=?,moved_clinic_address=?,moved_clinic_city=? " +
          "WHERE account_id=? AND reservation_id=? AND start_at=? AND state='pending'",
        newState, if (newState == "confirmed") Long.box(reviewedAt) else null, Long.box(reviewedAt),
        normalizedOperator, normalizedReason, action,
        moved.map(f => Long.box(f.startAt)).orNull, moved.map(f => Long.box(f.endAt)).orNull,
        moved.flatMap(_.clinicId).map(Long.box).orNull, moved.map(f => Boolean.box(f.telemedicine)).orNull,
        moved.flatMap(_.clinicAddress).orNull, moved.flatMap(_.clinicCity).orNull,
        Long.box(accountId), Long.box(reservationId), Long.box(startAt))
      if (changed != 1) throw new BookingRejectedException("CANCELLATION_REVIEW_TARGET_CHANGED")
      jdbc.update(
        "INSERT INTO cancellation_receipt_review_audit(account_id,reservation_id,start_at,old_state,new_state,action,reviewed_by,reason,reviewed_at," +
          "moved_start_at,moved_end_at,moved_clinic_id,moved_telemedicine,moved_clinic_address,moved_clinic_city) " +
          "VALUES (?,?,?,'pending',?,?,?,?,?,?,?,?,?,?,?)",
        Long.box(accountId), Long.box(reservationId), Long.box(startAt), newState, action,
        normalizedOperator, normalizedReason, Long.box(reviewedAt),
        moved.map(f => Long.box(f.startAt)).orNull, moved.map(f => Long.box(f.endAt)).orNull,
        moved.flatMap(_.clinicId).map(Long.box).orNull, moved.map(f => Boolean.box(f.telemedicine)).orNull,
        moved.flatMap(_.clinicAddress).orNull, moved.flatMap(_.clinicCity).orNull)
      receipt(accountId, reservationId).get
    })
  }

  def hasPending(accountId: Long): Boolean =
    jdbc.queryForObject("SELECT COUNT(*) FROM cancellation_receipt WHERE account_id=? AND " +
      "(state='pending' OR (state='verified_moved' AND moved_acknowledged_at IS NULL))",
      classOf[java.lang.Long], Long.box(accountId)) > 0

  def receipts(accountId: Long): Seq[CancellationReceipt] =
    jdbc.query(
      "SELECT account_id,reservation_id,start_at,state,confirmed_at,reviewed_at,reviewed_by,review_reason,review_action," +
        "moved_start_at,moved_end_at,moved_clinic_id,moved_telemedicine,moved_clinic_address,moved_clinic_city,moved_acknowledged_at " +
        "FROM cancellation_receipt WHERE account_id=? ORDER BY requested_at,reservation_id",
      (rs, _) => CancellationReceipt(rs.getLong("account_id"), rs.getLong("reservation_id"),
        rs.getLong("start_at"), rs.getString("state"),
        Option(rs.getObject("confirmed_at")).map(_.asInstanceOf[Number].longValue()),
        Option(rs.getObject("reviewed_at")).map(_.asInstanceOf[Number].longValue()),
        Option(rs.getString("reviewed_by")), Option(rs.getString("review_reason")),
        Option(rs.getString("review_action")),
        Option(rs.getObject("moved_start_at")).map(_.asInstanceOf[Number].longValue()),
        Option(rs.getObject("moved_end_at")).map(_.asInstanceOf[Number].longValue()),
        Option(rs.getObject("moved_clinic_id")).map(_.asInstanceOf[Number].longValue()),
        Option(rs.getObject("moved_telemedicine")).map(_.asInstanceOf[Boolean]),
        Option(rs.getString("moved_clinic_address")), Option(rs.getString("moved_clinic_city")),
        Option(rs.getObject("moved_acknowledged_at")).map(_.asInstanceOf[Number].longValue())),
      Long.box(accountId)).asScala.toSeq

  private def receipt(accountId: Long, reservationId: Long): Option[CancellationReceipt] =
    receipts(accountId).find(_.reservationId == reservationId)
}
