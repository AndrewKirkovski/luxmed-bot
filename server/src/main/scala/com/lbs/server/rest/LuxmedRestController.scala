package com.lbs.server.rest

import com.lbs.api.exception.InvalidLoginOrPasswordException
import com.lbs.api.json.model._
import com.lbs.bot.model.{MessageSource, MessageSourceSystem}
import com.lbs.server.repository.model.Monitoring
import com.lbs.server.service.{AccountBookingFence, ApiService, BookingCancellationService, DataService, MonitoringService}
import com.lbs.server.util.ServerModelConverters._
import com.typesafe.scalalogging.StrictLogging
import org.jasypt.util.text.TextEncryptor
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.{HttpStatus, ResponseEntity}
import org.springframework.web.bind.annotation._

import java.time.{Instant, LocalDateTime, LocalTime, OffsetDateTime, ZoneId, ZonedDateTime}
import scala.util.control.NonFatal

case class CancellationReviewRequest(expectedStartAt: Long, action: String, operator: String, reason: String,
                                     providerRequestSettled: Boolean = false,
                                     cancellationStatusVerified: Boolean = false,
                                     expectedMovedStartAt: Option[Long] = None)
case class CancellationMoveAcknowledgeRequest(expectedStartAt: Long, expectedMovedStartAt: Long)

@RestController
@RequestMapping(Array("/api/v1"))
class LuxmedRestController extends StrictLogging {

  @Autowired
  private var apiService: ApiService = _
  @Autowired
  private var dataService: DataService = _
  @Autowired
  private var monitoringService: MonitoringService = _
  @Autowired
  private var textEncryptor: TextEncryptor = _
  @Autowired
  private var bookingAttempts: BookingAttemptService = _
  @Autowired
  private var bookingFence: AccountBookingFence = _
  @Autowired
  private var bookingCancellation: BookingCancellationService = _

  private val RestSourceSystemId: Long = 3

  // === Health ===

  @GetMapping(Array("/health"))
  def health(): ResponseEntity[ApiResponse[String]] = {
    ResponseEntity.ok(ApiResponse.ok("ok"))
  }

  @GetMapping(Array("/capabilities"))
  def capabilities(): ResponseEntity[_] =
    ResponseEntity.ok(ApiResponse.ok(List("smart-booking-v1", "reservation-end-times-v1", "smart-booking-attempts-v2", "smart-booking-attempts-v3", "smart-booking-attempts-v4", "smart-booking-lockterm-review-v1", "monitor-quiesce-v1", "reservation-range-complete-v1", "legacy-monitor-fence-v1", "legacy-booking-barrier-v1", "legacy-booking-barrier-v2", "smart-booking-enrollment-fence-v1", "smart-booking-enrollment-fence-v2", "smart-booking-identity-fence-v1", "cancellation-receipts-v2", "cancellation-receipts-v3")))

  @GetMapping(Array("/accounts/{accountId}/smart-booking-enrollment"))
  def smartBookingEnrollment(@PathVariable accountId: Long): ResponseEntity[_] =
    ResponseEntity.ok(ApiResponse.ok(SmartBookingEnrollment(bookingAttempts.smartBookingEnrolled(accountId))))

  @PostMapping(Array("/accounts/{accountId}/smart-booking-enrollment"))
  def enrollSmartBooking(@PathVariable accountId: Long,
                         @RequestBody request: SmartBookingEnrollmentRequest): ResponseEntity[_] = bookingFence.withLock(accountId) {
    try {
      val expected = Option(request).flatMap(r => Option(r.expectedAutoMonitorIds))
        .getOrElse(throw new BookingRejectedException("INVALID_MONITOR_SET"))
      val stopped = bookingAttempts.enrollSmartBooking(accountId, expected)
      stopped.foreach(id => monitoringService.deactivateMonitoring(accountId, id))
      ResponseEntity.ok(ApiResponse.ok(SmartBookingEnrollment(true, stopped)))
    } catch {
      case ex: BookingRejectedException => ResponseEntity.status(HttpStatus.CONFLICT).body(ApiResponse.fail(ex.code))
    }
  }

  @GetMapping(Array("/accounts/{accountId}/booking-attempts/{attemptId}"))
  def bookingAttempt(@PathVariable accountId: Long, @PathVariable attemptId: String): ResponseEntity[_] =
    ResponseEntity.ok(ApiResponse.ok(bookingAttempts.status(accountId, attemptId).getOrElse(BookingOutcome("not_found"))))

  @GetMapping(Array("/accounts/{accountId}/booking-attempts/{attemptId}/review-context"))
  def bookingAttemptReviewContext(@PathVariable accountId: Long, @PathVariable attemptId: String): ResponseEntity[_] =
    bookingAttempts.reviewContext(accountId, attemptId) match {
      case Some(context) => ResponseEntity.ok(ApiResponse.ok(context))
      case None => ResponseEntity.status(HttpStatus.NOT_FOUND).body(ApiResponse.fail("Booking review context is unavailable"))
    }

  @PostMapping(Array("/accounts/{accountId}/booking-attempts/{attemptId}/review"))
  def reviewBookingAttempt(@PathVariable accountId: Long, @PathVariable attemptId: String,
                           @RequestBody request: BookingPositiveReviewRequest): ResponseEntity[_] =
    bookingFence.withLock(accountId) {
      try {
        val context = bookingAttempts.reviewContext(accountId, attemptId).getOrElse(
          throw new BookingRejectedException("BOOKING_REVIEW_CONTEXT_MISSING"))
        if (request == null || request.expectedStartAt != context.startAt ||
            request.expectedEndAt != context.endAt || request.expectedClinicId != context.clinicId ||
            request.expectedFingerprint != context.fingerprint)
          throw new BookingRejectedException("BOOKING_REVIEW_TARGET_CHANGED")
        val day = ZonedDateTime.ofInstant(Instant.ofEpochMilli(context.startAt), ZoneId.of("Europe/Warsaw"))
          .toLocalDate.atStartOfDay(ZoneId.of("Europe/Warsaw"))
        apiService.reservedVerified(accountId, day, day.plusDays(1)) match {
          case Right(observed) => ResponseEntity.ok(ApiResponse.ok(
            bookingAttempts.reviewPositiveAttempt(accountId, attemptId, request, observed)))
          case Left(error) => handleResult(Left(error))
        }
      } catch {
        case ex: BookingRejectedException => ResponseEntity.status(HttpStatus.CONFLICT).body(ApiResponse.fail(ex.code))
      }
    }

  @PostMapping(Array("/accounts/{accountId}/booking-attempts/{attemptId}/acknowledge"))
  def acknowledgeBookingAttempt(@PathVariable accountId: Long, @PathVariable attemptId: String,
                                @RequestBody request: SmartBookingAcknowledgeRequest): ResponseEntity[_] =
    bookingFence.withLock(accountId) {
      if (request != null && bookingAttempts.acknowledgeSmartBooking(accountId, attemptId, request.reservationId))
        ResponseEntity.ok(ApiResponse.ok(BookingOutcome("succeeded", Some(request.reservationId))))
      else ResponseEntity.status(HttpStatus.CONFLICT).body(ApiResponse.fail("Matching successful booking attempt was not found"))
    }

  @GetMapping(Array("/accounts/{accountId}/legacy-booking-barrier"))
  def legacyBookingBarrier(@PathVariable accountId: Long): ResponseEntity[_] =
    ResponseEntity.ok(ApiResponse.ok(bookingAttempts.legacyBarrier(accountId)))

  @PostMapping(Array("/accounts/{accountId}/legacy-booking-barrier/acknowledge"))
  def acknowledgeLegacyBooking(
    @PathVariable accountId: Long,
    @RequestBody request: LegacyBookingAcknowledgeRequest
  ): ResponseEntity[_] = bookingFence.withLock(accountId) {
    if (request != null && (request.id match {
        case Some(id) => request.expectedStartAt.exists(start =>
          bookingAttempts.acknowledgeLegacyBooking(accountId, id, request.reservationId, start))
        case None if request.expectedStartAt.isEmpty =>
          bookingAttempts.acknowledgeLegacyBookingV1(accountId, request.reservationId)
        case None => false
      }))
      ResponseEntity.ok(ApiResponse.ok("Acknowledged"))
    else ResponseEntity.status(HttpStatus.CONFLICT).body(ApiResponse.fail("Matching verified legacy booking barrier was not found"))
  }

  // === Authentication ===

  @PostMapping(Array("/login"))
  def login(@RequestBody request: LoginRequest): ResponseEntity[_] = {
    val encryptedPassword = textEncryptor.encrypt(request.password)
    apiService.fullLogin(request.username, encryptedPassword) match {
      case Right(session) =>
        try {
          val source = MessageSource(MessageSourceSystem(RestSourceSystemId), request.chatId)
          val credentials = dataService.saveCredentials(source, request.username, encryptedPassword)
          apiService.addSession(credentials.accountId, session)
          ResponseEntity.ok(ApiResponse.ok(LoginResult(credentials.userId, credentials.accountId, credentials.username)))
        } catch {
          case ex: BookingRejectedException => ResponseEntity.status(HttpStatus.CONFLICT).body(ApiResponse.fail(ex.code))
        }
      case Left(ex: InvalidLoginOrPasswordException) =>
        ResponseEntity.status(HttpStatus.UNAUTHORIZED).body(ApiResponse.fail("Invalid login or password"))
      case Left(ex) =>
        ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(ApiResponse.fail(Option(ex.getMessage).getOrElse(ex.getClass.getSimpleName)))
    }
  }

  // === Dictionary Data ===

  @GetMapping(Array("/accounts/{accountId}/cities"))
  def getCities(@PathVariable accountId: Long): ResponseEntity[_] = {
    handleResult(apiService.getAllCities(accountId))
  }

  @GetMapping(Array("/accounts/{accountId}/services"))
  def getServices(@PathVariable accountId: Long): ResponseEntity[_] = {
    handleResult(apiService.getAllServices(accountId))
  }

  @GetMapping(Array("/accounts/{accountId}/facilities"))
  def getFacilities(
    @PathVariable accountId: Long,
    @RequestParam cityId: Long,
    @RequestParam serviceId: Long
  ): ResponseEntity[_] = {
    handleResult(apiService.getAllFacilities(accountId, cityId, serviceId))
  }

  @GetMapping(Array("/accounts/{accountId}/doctors"))
  def getDoctors(
    @PathVariable accountId: Long,
    @RequestParam cityId: Long,
    @RequestParam serviceId: Long
  ): ResponseEntity[_] = {
    handleResult(apiService.getAllDoctors(accountId, cityId, serviceId))
  }

  // === Terms / Search ===

  @PostMapping(Array("/accounts/{accountId}/terms/search"))
  def searchTerms(
    @PathVariable accountId: Long,
    @RequestBody request: SearchTermsRequest
  ): ResponseEntity[_] = {
    val fromDate = LocalDateTime.parse(request.dateFrom)
    val toDate = LocalDateTime.parse(request.dateTo)
    val timeFrom = LocalTime.parse(request.timeFrom)
    val timeTo = LocalTime.parse(request.timeTo)

    handleResult(apiService.getAvailableTerms(
      accountId,
      request.cityId,
      request.clinicId,
      request.serviceId,
      request.doctorId,
      fromDate,
      toDate,
      timeFrom,
      timeTo
    ))
  }

  // === Booking (single-step) ===

  @PostMapping(Array("/accounts/{accountId}/book"))
  def bookAppointment(
    @PathVariable accountId: Long,
    @RequestBody request: BookRequest
  ): ResponseEntity[_] = {
    try bookingFence.withLock(accountId) {
      if (request.attemptId.nonEmpty)
        ResponseEntity.badRequest().body(ApiResponse.fail("Use booking-attempts for an idempotent booking request"))
      else if (bookingAttempts.identityHasSmartEnrollment(accountId))
        ResponseEntity.status(HttpStatus.CONFLICT).body(ApiResponse.fail("Smart booking enrollment blocks legacy booking for this account"))
        else if (bookingAttempts.accountBusy(accountId))
        ResponseEntity.status(HttpStatus.CONFLICT).body(ApiResponse.fail("Booking outcome is unresolved for this account"))
      else if (bookingAttempts.legacyBarrier(accountId).state != "clear")
        ResponseEntity.status(HttpStatus.CONFLICT).body(ApiResponse.fail("Legacy booking outcome is unresolved for this account"))
      else {
        val start = parseZonedDateTime(request.dateTimeFrom).toInstant.toEpochMilli
        val barrierId = bookingAttempts.beginLegacyBooking(accountId, start)
        var confirmationStarted = false
        val result = try performBooking(accountId, request, barrierId, () =>
          bookingAttempts.markLegacyConfirmationStarted(accountId, barrierId).map { _ => confirmationStarted = true })
        catch { case NonFatal(error) => Left(error) }
        val confirmed = result match {
          case Right(response) if !response.hasErrors && response.value != null && response.value.reservationId > 0 =>
            bookingAttempts.completeLegacyBooking(accountId, barrierId, response.value.reservationId)
            true
          case Left(_) if !confirmationStarted =>
            bookingAttempts.clearLegacyBeforeSubmission(accountId, barrierId)
            false
          case _ => false // A lost or incomplete confirmation remains pending until verified.
        }
        if (result.isRight && !confirmed)
          ResponseEntity.status(HttpStatus.CONFLICT).body(ApiResponse.fail("Legacy booking confirmation requires verification"))
        else handleResult(result)
      }
    } catch {
      case ex: BookingRejectedException => ResponseEntity.status(HttpStatus.CONFLICT).body(ApiResponse.fail(ex.code))
    }
  }

  /** Old bot images receive a definite failure without reaching LuxMed. */
  @PostMapping(Array("/accounts/{accountId}/booking-attempts"))
  def rejectOldSmartBookingAttempt(@PathVariable accountId: Long): ResponseEntity[_] =
    ResponseEntity.ok(ApiResponse.ok(BookingOutcome("failed", errorCode = Some("BOT_UPGRADE_REQUIRED"))))

  /** The versioned path prevents a new bot from submitting to an older sidecar after a rollback. */
  @PostMapping(Array("/accounts/{accountId}/booking-attempts/v4"))
  def submitBookingAttempt(
    @PathVariable accountId: Long,
    @RequestBody request: BookRequest
  ): ResponseEntity[_] = {
    if (request.attemptId.isEmpty)
      ResponseEntity.badRequest().body(ApiResponse.fail("Booking attempt ID is required"))
    else bookingFence.withLock(accountId) {
      ResponseEntity.ok(ApiResponse.ok(bookingAttempts.execute(accountId, request)(() => {
        if (request.rebookIfExists)
          Left(new BookingRejectedException("SMART_REBOOK_REQUIRES_REVIEW"))
        else if (bookingAttempts.cancellationPending(accountId))
          Left(new BookingRejectedException("CANCELLATION_UNRESOLVED"))
        else if (!request.isImpediment.contains(false) || request.impedimentText.exists(_.trim.nonEmpty))
          Left(new BookingRejectedException("IMPEDIMENT_REQUIRES_REVIEW"))
        else if (request.isPreparationRequired && request.preparationItems.isEmpty)
          Left(new BookingRejectedException("PREPARATION_MISSING"))
        else {
          val enrollment = try Right((bookingAttempts.smartBookingEnrolled(accountId),
            bookingAttempts.smartBookingIdentitySafe(accountId)))
          catch { case NonFatal(error) => Left(new BookingNotSubmittedException(error)) }
          enrollment match {
            case Left(error) => Left(error)
            case Right((false, _)) => Left(new BookingRejectedException("SMART_BOOKING_NOT_ENROLLED"))
            case Right((true, false)) => Left(new BookingRejectedException("SMART_BOOKING_IDENTITY_UNSAFE"))
            case Right((true, true)) =>
              val legacyActive = try Right(monitoringService.getActiveMonitorings(accountId).exists(_.autobook))
              catch { case NonFatal(error) => Left(new BookingNotSubmittedException(error)) }
              legacyActive match {
                case Left(error) => Left(error)
                case Right(true) => Left(new BookingRejectedException("LEGACY_AUTO_MONITOR_ACTIVE"))
                case Right(false) =>
                  val barrier = try Right(bookingAttempts.legacyBarrier(accountId))
                  catch { case NonFatal(error) => Left(new BookingNotSubmittedException(error)) }
                  barrier match {
                    case Left(error) => Left(error)
                    case Right(value) if value.state != "clear" => Left(new BookingRejectedException("LEGACY_BOOKING_BARRIER"))
                    case Right(_) => performBooking(accountId, request, request.attemptId.get, () => {
                      val day = parseZonedDateTime(request.dateTimeFrom).withZoneSameInstant(ZoneId.of("Europe/Warsaw")).toLocalDate
                      val from = day.minusDays(1).atStartOfDay(ZoneId.of("Europe/Warsaw"))
                      val to = day.plusDays(2).atStartOfDay(ZoneId.of("Europe/Warsaw"))
                      apiService.reservedVerified(accountId, from, to)
                        .flatMap { observed =>
                          BookingAttemptService.verifyReservationBaseline(request, observed,
                            from.toInstant.toEpochMilli, to.toInstant.toEpochMilli)
                            .flatMap(_ => bookingAttempts.markSmartConfirmationStarted(accountId, request.attemptId.get))
                        }
                    })
                  }
              }
          }
        }
      })))
    }
  }

  private def performBooking(accountId: Long, request: BookRequest, ownerId: String,
                             onConfirmationStarted: () => Either[Throwable, Unit] = () => Right(())): Either[Throwable, ReservationConfirmResponse] = {
    val termExt = buildTermExt(request)
    var confirmationStarted = false

    val result = for {
      xsrfToken <- apiService.getXsrfToken(accountId)
      locktermResponse <- apiService.reservationLockterm(accountId, xsrfToken, termExt.mapTo[ReservationLocktermRequest])
      _ <- BookingAttemptService.validateLockterm(locktermResponse, request.rebookIfExists,
        request.attemptId.map(_ => request.doctorId)).left.map { error =>
        Option(locktermResponse.value).filter(_.temporaryReservationId > 0).foreach(value =>
          apiService.deleteTemporaryReservation(accountId, xsrfToken, value.temporaryReservationId)
        )
        error
      }
      temporaryReservationId = locktermResponse.value.temporaryReservationId
      _ <- onConfirmationStarted().left.map { error =>
        try apiService.deleteTemporaryReservation(accountId, xsrfToken, temporaryReservationId)
        catch { case NonFatal(_) => () }
        error
      }
      response <- {
        confirmationStarted = true
        if (locktermResponse.value.changeTermAvailable && request.rebookIfExists) {
          logger.info(s"Service already booked. Trying to change term")
          bookOrUnlockTerm(
            accountId, xsrfToken, temporaryReservationId,
            apiService.reservationChangeTerm(_, xsrfToken, (locktermResponse, termExt).mapTo[ReservationChangetermRequest],
              ownerId)
          )
        } else {
          bookOrUnlockTerm(
            accountId, xsrfToken, temporaryReservationId,
            apiService.reservationConfirm(_, xsrfToken, (locktermResponse, termExt).mapTo[ReservationConfirmRequest],
              ownerId)
          )
        }
      }
    } yield response

    result.left.map(ex =>
      if (confirmationStarted && request.attemptId.nonEmpty)
        new RuntimeException("Booking confirmation outcome requires verification", ex)
      else if (request.attemptId.isEmpty || ex.isInstanceOf[BookingRejectedException]) ex
      else new BookingNotSubmittedException(ex)
    )
  }

  private def bookOrUnlockTerm[T](
    accountId: Long,
    xsrfToken: XsrfToken,
    temporaryReservationId: Long,
    fn: Long => Either[Throwable, T]
  ): Either[Throwable, T] = {
    fn(accountId) match {
      case r@Left(_) =>
        apiService.deleteTemporaryReservation(accountId, xsrfToken, temporaryReservationId)
        r
      case r => r
    }
  }

  // === Visits ===

  @GetMapping(Array("/accounts/{accountId}/visits/cancellation-receipts"))
  def cancellationReceipts(@PathVariable accountId: Long): ResponseEntity[_] =
    ResponseEntity.ok(ApiResponse.ok(bookingCancellation.cancellationReceipts(accountId)))

  @PostMapping(Array("/accounts/{accountId}/visits/cancellation-receipts/{reservationId}/review"))
  def reviewCancellation(@PathVariable accountId: Long, @PathVariable reservationId: Long,
                         @RequestBody request: CancellationReviewRequest): ResponseEntity[_] = {
    try {
      val reviewed = bookingCancellation.review(accountId, reservationId,
        Option(request).map(_.expectedStartAt).getOrElse(0L),
        Option(request).map(_.action).orNull,
        Option(request).map(_.operator).orNull,
        Option(request).map(_.reason).orNull,
        Option(request).exists(_.providerRequestSettled),
        Option(request).exists(_.cancellationStatusVerified),
        Option(request).flatMap(_.expectedMovedStartAt))
      ResponseEntity.ok(ApiResponse.ok(reviewed))
    } catch {
      case ex: BookingRejectedException => ResponseEntity.status(HttpStatus.CONFLICT).body(ApiResponse.fail(ex.code))
      case NonFatal(ex) => ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
        .body(ApiResponse.fail(Option(ex.getMessage).getOrElse(ex.getClass.getSimpleName)))
    }
  }

  @PostMapping(Array("/accounts/{accountId}/visits/cancellation-receipts/{reservationId}/acknowledge-move"))
  def acknowledgeMovedCancellation(@PathVariable accountId: Long, @PathVariable reservationId: Long,
                                   @RequestBody request: CancellationMoveAcknowledgeRequest): ResponseEntity[_] = {
    try {
      if (request != null && bookingCancellation.acknowledgeMove(accountId, reservationId,
          request.expectedStartAt, request.expectedMovedStartAt))
        ResponseEntity.ok(ApiResponse.ok("Acknowledged"))
      else ResponseEntity.status(HttpStatus.CONFLICT).body(ApiResponse.fail("Matching moved reservation was not found"))
    } catch {
      case ex: BookingRejectedException => ResponseEntity.status(HttpStatus.CONFLICT).body(ApiResponse.fail(ex.code))
    }
  }

  @GetMapping(Array("/accounts/{accountId}/visits/reserved"))
  def getReservedVisits(@PathVariable accountId: Long): ResponseEntity[_] = {
    handleResult(apiService.reserved(accountId))
  }

  @GetMapping(Array("/accounts/{accountId}/visits/reserved/verified"))
  def getVerifiedReservedVisits(
    @PathVariable accountId: Long,
    @RequestParam from: String,
    @RequestParam to: String
  ): ResponseEntity[_] = {
    handleResult(apiService.reservedVerified(accountId, parseZonedDateTime(from), parseZonedDateTime(to)))
  }

  @GetMapping(Array("/accounts/{accountId}/visits/history"))
  def getHistory(@PathVariable accountId: Long): ResponseEntity[_] = {
    handleResult(apiService.history(accountId))
  }

  @DeleteMapping(Array("/accounts/{accountId}/visits/{reservationId}"))
  def cancelVisit(
    @PathVariable accountId: Long,
    @PathVariable reservationId: Long,
    @RequestParam(required = false) expectedStartAt: java.lang.Long = null
  ): ResponseEntity[_] = {
    bookingCancellation.cancel(accountId, reservationId, Option(expectedStartAt).map(_.longValue())) match {
      case Right(_) => ResponseEntity.ok(ApiResponse.ok("Cancelled"))
      case Left(ex: BookingRejectedException) => ResponseEntity.status(HttpStatus.CONFLICT).body(ApiResponse.fail(ex.code))
      case Left(ex) => ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(ApiResponse.fail(Option(ex.getMessage).getOrElse(ex.getClass.getSimpleName)))
    }
  }

  // === Monitoring ===

  @GetMapping(Array("/accounts/{accountId}/monitorings"))
  def getMonitorings(@PathVariable accountId: Long): ResponseEntity[_] = {
    val monitorings = monitoringService.getActiveMonitorings(accountId)
    ResponseEntity.ok(ApiResponse.ok(monitorings))
  }

  @PostMapping(Array("/accounts/{accountId}/monitorings"))
  def createMonitoring(
    @PathVariable accountId: Long,
    @RequestBody request: CreateMonitoringRequest
  ): ResponseEntity[_] = {
    val credentialsMaybe = dataService.getCredentials(accountId)
    credentialsMaybe match {
      case Some(credentials) =>
        val monitoring = Monitoring(
          userId = credentials.userId,
          username = credentials.username,
          accountId = accountId,
          chatId = request.chatId,
          sourceSystemId = RestSourceSystemId,
          payerId = request.payerId,
          cityId = request.cityId,
          cityName = request.cityName,
          clinicId = request.clinicId,
          clinicName = request.clinicName,
          serviceId = request.serviceId,
          serviceName = request.serviceName,
          doctorId = request.doctorId,
          doctorName = request.doctorName,
          dateFrom = parseZonedDateTime(request.dateFrom),
          dateTo = parseZonedDateTime(request.dateTo),
          timeFrom = LocalTime.parse(request.timeFrom),
          timeTo = LocalTime.parse(request.timeTo),
          autobook = request.autobook,
          rebookIfExists = request.rebookIfExists,
          offset = request.offset
        )
        try {
          val saved = monitoringService.createMonitoring(monitoring)
          ResponseEntity.ok(ApiResponse.ok(saved))
        } catch {
          case ex: BookingRejectedException => ResponseEntity.status(HttpStatus.CONFLICT).body(ApiResponse.fail(ex.code))
        }
      case None =>
        ResponseEntity.status(HttpStatus.NOT_FOUND).body(ApiResponse.fail(s"No credentials found for account $accountId"))
    }
  }

  @DeleteMapping(Array("/accounts/{accountId}/monitorings/{monitoringId}"))
  def deactivateMonitoring(
    @PathVariable accountId: Long,
    @PathVariable monitoringId: Long
  ): ResponseEntity[_] = {
    monitoringService.deactivateMonitoring(accountId, monitoringId)
    ResponseEntity.ok(ApiResponse.ok("Deactivated"))
  }

  @PostMapping(Array("/accounts/{accountId}/monitorings/{monitoringId}/quiesce"))
  def quiesceMonitoring(
    @PathVariable accountId: Long,
    @PathVariable monitoringId: Long
  ): ResponseEntity[_] = {
    monitoringService.deactivateMonitoring(accountId, monitoringId)
    ResponseEntity.ok(ApiResponse.ok("Quiesced"))
  }

  // === Helpers ===

  private def handleResult[T](result: Either[Throwable, T]): ResponseEntity[_] = {
    result match {
      case Right(data) => ResponseEntity.ok(ApiResponse.ok(data))
      case Left(ex: InvalidLoginOrPasswordException) =>
        ResponseEntity.status(HttpStatus.UNAUTHORIZED).body(ApiResponse.fail("Invalid login or password"))
      case Left(ex) =>
        logger.error("API call failed", ex)
        ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(ApiResponse.fail(Option(ex.getMessage).getOrElse(ex.getClass.getSimpleName)))
    }
  }

  private def buildTermExt(request: BookRequest): TermExt = {
    val dateTimeFrom = LuxmedFunnyDateTime(dateTimeLocal = Some(parseDateTime(request.dateTimeFrom)))
    val dateTimeTo = LuxmedFunnyDateTime(dateTimeLocal = Some(parseDateTime(request.dateTimeTo)))
    def optional(value: String): Option[String] = Option(value).map(_.trim).filter(_.nonEmpty)
    val doctor = Doctor(
      academicTitle = optional(request.doctorAcademicTitle),
      facilityGroupIds = None,
      firstName = optional(request.doctorFirstName),
      isEnglishSpeaker = None,
      genderId = None,
      id = request.doctorId,
      lastName = optional(request.doctorLastName)
    )
    val term = Term(
      clinic = optional(request.clinic),
      clinicId = request.clinicId,
      clinicGroupId = request.clinicGroupId,
      dateTimeFrom = dateTimeFrom,
      dateTimeTo = dateTimeTo,
      doctor = doctor,
      impedimentText = request.impedimentText,
      isAdditional = request.isAdditional,
      isImpediment = request.isImpediment.getOrElse(false),
      isTelemedicine = request.isTelemedicine,
      roomId = request.roomId,
      scheduleId = request.scheduleId,
      serviceId = request.serviceId
    )
    val additionalData = AdditionalData(
      isPreparationRequired = request.isPreparationRequired,
      preparationItems = request.preparationItems
    )
    TermExt(additionalData, term)
  }

  private def parseDateTime(value: String): LocalDateTime = {
    try {
      OffsetDateTime.parse(value).atZoneSameInstant(ZoneId.of("Europe/Warsaw")).toLocalDateTime
    } catch {
      case _: java.time.format.DateTimeParseException => LocalDateTime.parse(value)
    }
  }

  private def parseZonedDateTime(value: String): ZonedDateTime = {
    try {
      ZonedDateTime.parse(value)
    } catch {
      case _: java.time.format.DateTimeParseException =>
        LocalDateTime.parse(value).atZone(ZoneId.of("Europe/Warsaw"))
    }
  }
}
