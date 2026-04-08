package com.lbs.server.rest

import com.lbs.api.exception.InvalidLoginOrPasswordException
import com.typesafe.scalalogging.StrictLogging
import org.springframework.http.{HttpStatus, ResponseEntity}
import org.springframework.web.bind.annotation.{ControllerAdvice, ExceptionHandler}

import java.time.format.DateTimeParseException

@ControllerAdvice
class RestExceptionHandler extends StrictLogging {

  @ExceptionHandler(Array(classOf[InvalidLoginOrPasswordException]))
  def handleAuthError(ex: InvalidLoginOrPasswordException): ResponseEntity[ApiResponse[Nothing]] = {
    ResponseEntity.status(HttpStatus.UNAUTHORIZED)
      .body(ApiResponse.fail("Invalid login or password"))
  }

  @ExceptionHandler(Array(classOf[DateTimeParseException]))
  def handleDateParseError(ex: DateTimeParseException): ResponseEntity[ApiResponse[Nothing]] = {
    ResponseEntity.status(HttpStatus.BAD_REQUEST)
      .body(ApiResponse.fail(s"Invalid date/time format: ${ex.getParsedString}"))
  }

  @ExceptionHandler(Array(classOf[IllegalArgumentException]))
  def handleBadRequest(ex: IllegalArgumentException): ResponseEntity[ApiResponse[Nothing]] = {
    ResponseEntity.status(HttpStatus.BAD_REQUEST)
      .body(ApiResponse.fail(Option(ex.getMessage).getOrElse("Invalid request")))
  }

  @ExceptionHandler(Array(classOf[Exception]))
  def handleGenericError(ex: Exception): ResponseEntity[ApiResponse[Nothing]] = {
    logger.error("REST API error", ex)
    ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
      .body(ApiResponse.fail(Option(ex.getMessage).getOrElse(ex.getClass.getSimpleName)))
  }
}
