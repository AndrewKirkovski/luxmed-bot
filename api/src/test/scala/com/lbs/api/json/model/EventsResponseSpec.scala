package com.lbs.api.json.model

import com.lbs.api.json.JsonSerializer.extensions.*
import com.lbs.api.json.model.JsonCodecs.given
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class EventsResponseSpec extends AnyFunSuite with Matchers {
  test("reserved visits retain their end time and clinic identity") {
    val response = """{"Events":[{"Date":"2026-10-06T11:00:00+02:00","DateTo":"2026-10-06T11:30:00+02:00","Clinic":{"Address":"Fixture 1","City":"Warszawa","Id":42,"Name":"Fixture clinic"},"Doctor":null,"EventId":77,"Status":"Reserved","Title":"Fixture","EventType":"Visit"}]}""".as[EventsResponse]
    val event = response.events.head
    event.dateTo.map(_.toLocalTime.toString) shouldBe Some("11:30")
    event.clinic.flatMap(_.id) shouldBe Some(42L)
    event.eventType shouldBe Some("Visit")
  }

  test("legacy responses preserve missing duration as unknown") {
    val response = """{"Events":[{"Date":"2026-10-06T11:00:00+02:00","Clinic":null,"Doctor":null,"EventId":77,"Status":"Reserved","Title":"Fixture"}]}""".as[EventsResponse]
    response.events.head.dateTo shouldBe None
    response.events.head.clinic shouldBe None
  }
}
