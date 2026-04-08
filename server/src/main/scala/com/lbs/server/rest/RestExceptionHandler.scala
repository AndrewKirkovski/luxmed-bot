package com.lbs.server.rest

import com.lbs.api.exception.InvalidLoginOrPasswordException
import com.typesafe.scalalogging.StrictLogging
import org.springframework.http.{HttpStatus, ResponseEntity}
import org.springframework.web.bind.annotation.{ControllerAdvice, ExceptionHandler}

@ControllerAdvice
class RestExceptionHandler extends StrictLogging {

  @ExceptionHandler(Array(classOf[InvalidLoginOrPasswordException]))
  def handleAuthError(ex: InvalidLoginOrPasswordException): ResponseEntity[ApiResponse[Nothing]] = {
    ResponseEntity.status(HttpStatus.UNAUTHORIZED)
      .body(ApiResponse.fail("Invalid login or password"))
  }

  @ExceptionHandler(Array(classOf[Exception]))
  def handleGenericError(ex: Exception): ResponseEntity[ApiResponse[Nothing]] = {
    logger.error("REST API error", ex)
    ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
      .body(ApiResponse.fail(ex.getMessage))
  }
}
