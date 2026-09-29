package com.lbs.server.rest

import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import org.springframework.web.bind.annotation.PostMapping

class BookingVersionedRouteSpec {
  @Test
  def smartBookingUsesDistinctVersionedPostPath(): Unit = {
    val route = classOf[LuxmedRestController].getDeclaredMethods
      .find(_.getName == "submitBookingAttempt")
      .flatMap(method => Option(method.getAnnotation(classOf[PostMapping])))
      .getOrElse(fail("Smart booking POST mapping is missing"))

    assertTrue(route.value().contains("/accounts/{accountId}/booking-attempts/v4"))
    assertFalse(route.value().contains("/accounts/{accountId}/booking-attempts"))

    val oldRoute = classOf[LuxmedRestController].getDeclaredMethods
      .find(_.getName == "rejectOldSmartBookingAttempt")
      .flatMap(method => Option(method.getAnnotation(classOf[PostMapping])))
      .getOrElse(fail("Old smart booking rejection route is missing"))
    assertTrue(oldRoute.value().contains("/accounts/{accountId}/booking-attempts"))
    val rejected = new LuxmedRestController().rejectOldSmartBookingAttempt(1L).getBody
      .asInstanceOf[ApiResponse[BookingOutcome]]
    assertEquals("failed", rejected.data.get.state)
    assertEquals(Some("BOT_UPGRADE_REQUIRED"), rejected.data.get.errorCode)
  }
}
