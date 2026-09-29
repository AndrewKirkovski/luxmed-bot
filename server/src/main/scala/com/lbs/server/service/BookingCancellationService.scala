package com.lbs.server.service

import com.lbs.server.ThrowableOr
import com.lbs.server.rest.BookingRejectedException
import com.lbs.server.util.DateTimeUtil
import org.springframework.stereotype.Service

/** Keeps cancellation and booking submissions in the same account critical section. */
@Service
class BookingCancellationService(apiService: ApiService, bookingAttempts: com.lbs.server.rest.BookingAttemptService,
                                 bookingFence: AccountBookingFence, receipts: CancellationReceiptService) {
  def cancellationReceipts(accountId: Long): Seq[CancellationReceipt] = receipts.receipts(accountId)

  def review(accountId: Long, reservationId: Long, startAt: Long,
              action: String, operator: String, reason: String,
              providerRequestSettled: Boolean, cancellationStatusVerified: Boolean,
              expectedMovedStartAt: Option[Long] = None): CancellationReceipt = bookingFence.withLock(accountId) {
    if (!providerRequestSettled) throw new BookingRejectedException("CANCELLATION_REVIEW_EVIDENCE_MISSING")
    if (action == "confirmed_cancelled" && !cancellationStatusVerified)
      throw new BookingRejectedException("CANCELLATION_STATUS_NOT_VERIFIED")
    if (startAt <= 0) throw new BookingRejectedException("INVALID_CANCELLATION_REVIEW")
    val day = java.time.Instant.ofEpochMilli(startAt).atZone(DateTimeUtil.Zone).toLocalDate
    val movedDay = if (action == "verified_moved") {
      val at = expectedMovedStartAt.getOrElse(throw new BookingRejectedException("MOVED_RESERVATION_START_REQUIRED"))
      if (at <= 0 || at == startAt) throw new BookingRejectedException("MOVED_RESERVATION_START_REQUIRED")
      Some(java.time.Instant.ofEpochMilli(at).atZone(DateTimeUtil.Zone).toLocalDate)
    } else None
    val today = java.time.LocalDate.now(DateTimeUtil.Zone)
    val fromDay = if (action == "confirmed_cancelled" && today.isBefore(day)) today.minusDays(1)
      else if (action == "confirmed_cancelled") day.minusDays(1)
      else movedDay.map(other => if (other.isBefore(day)) other else day).getOrElse(day)
    val toDay = if (action == "confirmed_cancelled" && today.isAfter(day)) today.plusYears(1)
      else if (action == "confirmed_cancelled") day.plusYears(1)
      else movedDay.map(other => if (other.isAfter(day)) other.plusDays(1) else day.plusDays(1))
        .getOrElse(day.plusDays(1))
    val observed = apiService.reservedVerified(accountId, fromDay.atStartOfDay(DateTimeUtil.Zone),
      toDay.atStartOfDay(DateTimeUtil.Zone)) match {
      case Right(events) => events
      case Left(_) => throw new BookingRejectedException("RESERVATION_FEED_UNVERIFIED")
    }
    bookingAttempts.reviewCancellation(accountId, reservationId, startAt, action, operator, reason,
      providerRequestSettled, cancellationStatusVerified, expectedMovedStartAt, observed)
  }

  def acknowledgeMove(accountId: Long, reservationId: Long, originalStartAt: Long,
                      movedStartAt: Long): Boolean = bookingFence.withLock(accountId) {
    bookingAttempts.acknowledgeMovedCancellation(accountId, reservationId, originalStartAt, movedStartAt)
  }

  def cancel(accountId: Long, reservationId: Long, expectedStartAt: Option[Long] = None): ThrowableOr[Unit] = bookingFence.withLock(accountId) {
    if (bookingAttempts.accountBusy(accountId) || bookingAttempts.legacyBarrier(accountId).state != "clear")
      Left(new BookingRejectedException("BOOKING_OUTCOME_UNRESOLVED"))
    else if (reservationId <= 0)
      Left(new BookingRejectedException("INVALID_RESERVATION_ID"))
    else {
      apiService.reserved(accountId).flatMap { upcoming =>
        val visit = upcoming.find(_.eventId == reservationId)
        if (visit.isEmpty)
          Left(new BookingRejectedException("RESERVATION_NOT_VERIFIED"))
        else if (expectedStartAt.exists(_ != visit.get.date.toInstant.toEpochMilli))
          Left(new BookingRejectedException("CANCELLATION_TARGET_CHANGED"))
        else {
          val day = visit.get.date.withZoneSameInstant(DateTimeUtil.Zone).toLocalDate
          val from = day.atStartOfDay(DateTimeUtil.Zone)
          val to = day.plusDays(1).atStartOfDay(DateTimeUtil.Zone)
          apiService.reservedVerified(accountId, from, to).flatMap { before =>
            val verifiedVisit = before.find(event => event.eventId == reservationId &&
              event.date.toInstant == visit.get.date.toInstant)
            if (verifiedVisit.isEmpty)
              Left(new BookingRejectedException("RESERVATION_NOT_VERIFIED"))
            else {
              val startAt = verifiedVisit.get.date.toInstant.toEpochMilli
              val started = try {
                bookingAttempts.beginCancellation(accountId, reservationId, startAt)
                Right(())
              } catch {
                case ex: BookingRejectedException => Left(ex)
              }
              started.flatMap { _ =>
                apiService.deleteReservation(accountId, reservationId).flatMap { response =>
                  // A DELETE response or an absent feed does not prove cancellation.
                  // The pending receipt remains until an operator records the outcome.
                  if (response.code < 200 || response.code >= 300 || Option(response.body).exists(_.trim.nonEmpty))
                    Left(new BookingRejectedException("CANCELLATION_RESPONSE_UNVERIFIED"))
                  else Left(new BookingRejectedException("CANCELLATION_REVIEW_REQUIRED"))
                }
              }
            }
          }
        }
      }
    }
  }
}
