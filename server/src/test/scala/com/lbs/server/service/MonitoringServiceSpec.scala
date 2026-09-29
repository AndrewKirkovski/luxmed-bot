package com.lbs.server.service

import com.lbs.server.repository.model.Monitoring
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import org.mockito.Mockito.{mock, verify, verifyNoInteractions}

import java.util.concurrent.ScheduledFuture
import scala.collection.mutable

class MonitoringServiceSpec {
  @Test def anotherAccountCannotDeactivateAScheduledMonitor(): Unit = {
    val monitoring = new Monitoring()
    monitoring.recordId = 10L
    monitoring.accountId = 1L
    monitoring.active = true
    val future = mock(classOf[ScheduledFuture[?]])
    val scheduled = mutable.Map[java.lang.Long, (Monitoring, ScheduledFuture[?])](Long.box(10L) -> (monitoring, future))
    val data = mock(classOf[DataService])
    val service = new MonitoringService()
    for ((name, value) <- List(
      "bookingFence" -> new AccountBookingFence(),
      "dataService" -> data,
      "activeMonitorings" -> scheduled
    )) {
      val field = classOf[MonitoringService].getDeclaredField(name)
      field.setAccessible(true)
      field.set(service, value)
    }

    service.deactivateMonitoring(2L, 10L)
    assertTrue(scheduled.contains(Long.box(10L)))
    assertTrue(monitoring.active)
    verifyNoInteractions(data, future)

    service.deactivateMonitoring(1L, 10L)
    assertFalse(scheduled.contains(Long.box(10L)))
    assertFalse(monitoring.active)
    verify(future).cancel(true)
    verify(data).saveMonitoring(monitoring)
  }
}
