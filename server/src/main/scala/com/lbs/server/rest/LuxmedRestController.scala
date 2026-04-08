package com.lbs.server.rest

import com.lbs.api.exception.InvalidLoginOrPasswordException
import com.lbs.api.json.model._
import com.lbs.bot.model.{MessageSource, MessageSourceSystem}
import com.lbs.server.repository.model.Monitoring
import com.lbs.server.service.{ApiService, DataService, MonitoringService}
import com.lbs.server.util.ServerModelConverters._
import com.typesafe.scalalogging.StrictLogging
import org.jasypt.util.text.TextEncryptor
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.{HttpStatus, ResponseEntity}
import org.springframework.web.bind.annotation._

import java.time.{LocalDateTime, LocalTime, ZonedDateTime}

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

  private val RestSourceSystemId: Long = 3

  // === Health ===

  @GetMapping(Array("/health"))
  def health(): ResponseEntity[ApiResponse[String]] = {
    ResponseEntity.ok(ApiResponse.ok("ok"))
  }

  // === Authentication ===

  @PostMapping(Array("/login"))
  def login(@RequestBody request: LoginRequest): ResponseEntity[_] = {
    val encryptedPassword = textEncryptor.encrypt(request.password)
    apiService.fullLogin(request.username, encryptedPassword) match {
      case Right(session) =>
        val source = MessageSource(MessageSourceSystem(RestSourceSystemId), request.chatId)
        val credentials = dataService.saveCredentials(source, request.username, encryptedPassword)
        apiService.addSession(credentials.accountId, session)
        ResponseEntity.ok(ApiResponse.ok(LoginResult(credentials.userId, credentials.accountId, credentials.username)))
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
    val termExt = buildTermExt(request)

    val result = for {
      xsrfToken <- apiService.getXsrfToken(accountId)
      locktermResponse <- apiService.reservationLockterm(accountId, xsrfToken, termExt.mapTo[ReservationLocktermRequest])
      temporaryReservationId = locktermResponse.value.temporaryReservationId
      response <- {
        if (locktermResponse.value.changeTermAvailable && request.rebookIfExists) {
          logger.info(s"Service already booked. Trying to change term")
          bookOrUnlockTerm(
            accountId, xsrfToken, temporaryReservationId,
            apiService.reservationChangeTerm(_, xsrfToken, (locktermResponse, termExt).mapTo[ReservationChangetermRequest])
          )
        } else {
          bookOrUnlockTerm(
            accountId, xsrfToken, temporaryReservationId,
            apiService.reservationConfirm(_, xsrfToken, (locktermResponse, termExt).mapTo[ReservationConfirmRequest])
          )
        }
      }
    } yield response

    handleResult(result)
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

  @GetMapping(Array("/accounts/{accountId}/visits/reserved"))
  def getReservedVisits(@PathVariable accountId: Long): ResponseEntity[_] = {
    handleResult(apiService.reserved(accountId))
  }

  @GetMapping(Array("/accounts/{accountId}/visits/history"))
  def getHistory(@PathVariable accountId: Long): ResponseEntity[_] = {
    handleResult(apiService.history(accountId))
  }

  @DeleteMapping(Array("/accounts/{accountId}/visits/{reservationId}"))
  def cancelVisit(
    @PathVariable accountId: Long,
    @PathVariable reservationId: Long
  ): ResponseEntity[_] = {
    apiService.deleteReservation(accountId, reservationId) match {
      case Right(_) => ResponseEntity.ok(ApiResponse.ok("Cancelled"))
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
          dateFrom = ZonedDateTime.parse(request.dateFrom),
          dateTo = ZonedDateTime.parse(request.dateTo),
          timeFrom = LocalTime.parse(request.timeFrom),
          timeTo = LocalTime.parse(request.timeTo),
          autobook = request.autobook,
          rebookIfExists = request.rebookIfExists,
          offset = request.offset
        )
        val saved = monitoringService.createMonitoring(monitoring)
        ResponseEntity.ok(ApiResponse.ok(saved))
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
    val dateTimeFrom = LuxmedFunnyDateTime(dateTimeLocal = Some(LocalDateTime.parse(request.dateTimeFrom)))
    val dateTimeTo = LuxmedFunnyDateTime(dateTimeLocal = Some(LocalDateTime.parse(request.dateTimeTo)))
    val doctor = Doctor(
      academicTitle = request.doctorAcademicTitle,
      facilityGroupIds = None,
      firstName = request.doctorFirstName,
      isEnglishSpeaker = None,
      genderId = None,
      id = request.doctorId,
      lastName = request.doctorLastName
    )
    val term = Term(
      clinic = request.clinic,
      clinicId = request.clinicId,
      clinicGroupId = request.clinicGroupId,
      dateTimeFrom = dateTimeFrom,
      dateTimeTo = dateTimeTo,
      doctor = doctor,
      impedimentText = "",
      isAdditional = request.isAdditional,
      isImpediment = false,
      isTelemedicine = request.isTelemedicine,
      roomId = request.roomId,
      scheduleId = request.scheduleId,
      serviceId = request.serviceId
    )
    val additionalData = AdditionalData(
      isPreparationRequired = request.isPreparationRequired,
      preparationItems = List.empty
    )
    TermExt(additionalData, term)
  }
}
