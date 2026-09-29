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
             action: String, operator: String, reason: String): CancellationReceipt = bookingFence.withLock(accountId) {
    receipts.review(accountId, reservationId, startAt, action, operator, reason)
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
                receipts.begin(accountId, reservationId, startAt)
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
