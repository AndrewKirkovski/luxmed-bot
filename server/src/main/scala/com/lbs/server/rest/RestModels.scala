package com.lbs.server.rest

case class LoginRequest(username: String, password: String, chatId: String)

case class SearchTermsRequest(
  cityId: Long,
  serviceId: Long,
  clinicId: Option[Long] = None,
  doctorId: Option[Long] = None,
  dateFrom: String,
  dateTo: String,
  timeFrom: String,
  timeTo: String
)

case class BookRequest(
  cityId: Long,
  clinicId: Long,
  clinicGroupId: Long,
  clinic: String,
  doctorId: Long,
  doctorFirstName: String,
  doctorLastName: String,
  doctorAcademicTitle: String,
  roomId: Long,
  scheduleId: Long,
  serviceId: Long,
  dateTimeFrom: String,
  dateTimeTo: String,
  isTelemedicine: Boolean = false,
  isAdditional: Boolean = false,
  isPreparationRequired: Boolean = false,
  rebookIfExists: Boolean = false
)

case class CreateMonitoringRequest(
  chatId: String,
  payerId: Long,
  cityId: Long,
  cityName: String,
  serviceId: Long,
  serviceName: String,
  clinicId: Option[Long] = None,
  clinicName: String = "Any",
  doctorId: Option[Long] = None,
  doctorName: String = "Any",
  dateFrom: String,
  dateTo: String,
  timeFrom: String,
  timeTo: String,
  autobook: Boolean = false,
  rebookIfExists: Boolean = false,
  offset: Int = 0
)

case class ApiResponse[T](success: Boolean, data: Option[T] = None, error: Option[String] = None)

object ApiResponse {
  def ok[T](data: T): ApiResponse[T] = ApiResponse(success = true, data = Some(data))
  def ok(): ApiResponse[Nothing] = ApiResponse(success = true)
  def fail(error: String): ApiResponse[Nothing] = ApiResponse(success = false, error = Some(error))
}

case class LoginResult(userId: Long, accountId: Long, username: String)
