package com.lbs.api.json.model

import com.lbs.api.json.JsonSerializer.extensions.*
import com.lbs.api.json.model.JsonCodecs.given
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class RehabModelsSpec extends AnyFunSuite with Matchers {

  test("ReferralsResponse decodes real NewPortal/Referrals/module payload") {
    val json =
      """{
        |  "referrals": [
        |    {
        |      "referralId": 999999902,
        |      "eReferralId": null,
        |      "specializationName": "Rehabilitacja",
        |      "referralSpecializationTag": "None",
        |      "tagTranslation": "",
        |      "doctorName": "mgr DOCTOR_FIRSTNAME DOCTOR_LASTNAME",
        |      "visitPreparation": "Stroj niekrepujacy ruchow.",
        |      "suggestedDueDate": null,
        |      "referralValidDate": "2026-10-29T00:00:00",
        |      "referralIssuedDate": "2026-05-02T10:36:11.267",
        |      "isOverdue": false,
        |      "isVisitAppointable": true,
        |      "isSuggestedTermPassed": false,
        |      "isTooEarlyToMakeAnAppointment": false,
        |      "earliestDateToMakeAnAppointment": null,
        |      "referralType": "Rehabilitation",
        |      "serviceInstanceId": 999999903,
        |      "proceduresAmount": 3,
        |      "procedures": [
        |        {
        |          "name": "KINEZYTERAPIA Terapia indywidualna - kregoslup 1 odcinek",
        |          "count": 3,
        |          "preparation": "Na czym polega zabieg?"
        |        }
        |      ],
        |      "howToBookInfo": null,
        |      "actions": [4],
        |      "renewableInformation": null,
        |      "packageVerification": null,
        |      "serviceVariantId": 12463,
        |      "tags": null,
        |      "eReferralAccessCode": null,
        |      "eReferralDocumentId": null
        |    }
        |  ]
        |}""".stripMargin

    val response = json.as[ReferralsResponse]

    response.referrals should have size 1

    val referral = response.referrals.head
    referral.referralId shouldBe Some(999999902L)
    referral.specializationName shouldBe "Rehabilitacja"
    referral.isVisitAppointable shouldBe true
    referral.referralType shouldBe "Rehabilitation"
    referral.serviceInstanceId shouldBe 999999903L
    referral.serviceVariantId shouldBe 12463L
    referral.proceduresAmount shouldBe 3
    referral.procedures.get should have size 1
    referral.procedures.get.head.name shouldBe "KINEZYTERAPIA Terapia indywidualna - kregoslup 1 odcinek"
    referral.procedures.get.head.count shouldBe 3
    referral.doctorName shouldBe Some("mgr DOCTOR_FIRSTNAME DOCTOR_LASTNAME")
    referral.referralValidDate shouldBe Some("2026-10-29T00:00:00")
  }

  test("RehabFacilitiesResponse decodes real API payload with camelCase normalization") {
    val json =
      """{
        |  "locations": [
        |    { "id": 999999905, "name": "Warszawa" },
        |    { "id": 999999906, "name": "Krakow" }
        |  ],
        |  "facilities": [
        |    { "id": 999999929, "name": "LX Stara Iwiczna - Nowa 4A", "locationId": 1, "availabilityLevel": 0 },
        |    { "id": 999999932, "name": "LX Warszawa - Gorczewska 124", "locationId": 1, "availabilityLevel": 0 },
        |    { "id": 999999936, "name": "LX Fizjoterapia Warszawa", "locationId": 100, "availabilityLevel": 0 }
        |  ],
        |  "description": "Get facilities for rehabilitation service"
        |}""".stripMargin

    val response = json.as[RehabFacilitiesResponse]

    response.locations should have size 2
    response.locations.head.name shouldBe "Warszawa"
    response.locations.head.id shouldBe 999999905L

    response.facilities should have size 3
    val warszawa = response.facilities.filter(_.name.contains("Gorczewska")).head
    warszawa.locationId shouldBe 1L
    warszawa.availabilityLevel shouldBe 0
  }

  test("TermsIndexResponse decodes real nextTerms payload with termsInfoForDays") {
    val json =
      """{
        |  "correlationId": "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx",
        |  "termsForService": {
        |    "serviceVariantId": 12463,
        |    "additionalData": {
        |      "isPreparationRequired": true,
        |      "preparationItems": [
        |        { "header": "Na czym polega zabieg?", "text": "Indywidualna praca..." },
        |        { "header": "Jak sie przygotowac?", "text": "Stroj niekrepujacy..." }
        |      ],
        |      "previousTermsAvailable": true,
        |      "nextTermsAvailable": true,
        |      "anyTermForTelemedicine": false,
        |      "anyTermForFacilityVisit": false
        |    },
        |    "termsForDays": [],
        |    "termsInfoForDays": [
        |      {
        |        "day": "2026-04-24T00:00:00",
        |        "termsStatus": 3,
        |        "message": "Wszystkie terminy zostaly zarezerwowane.",
        |        "isLimitedDay": false,
        |        "isLastDayWithLoadedTerms": false,
        |        "isMoreTermsThanCounter": null,
        |        "termsCounter": { "termsNumber": 0, "partialTermsCounters": [] }
        |      },
        |      {
        |        "day": "2026-04-28T00:00:00",
        |        "termsStatus": 0,
        |        "message": "W tym dniu dostepne sa terminy.",
        |        "isLimitedDay": true,
        |        "isLastDayWithLoadedTerms": false,
        |        "isMoreTermsThanCounter": null,
        |        "termsCounter": {
        |          "termsNumber": 1,
        |          "partialTermsCounters": [
        |            { "clinicGroupId": 2767, "doctorId": 11106, "priority": 1, "termsNumber": 1 }
        |          ]
        |        }
        |      }
        |    ]
        |  },
        |  "pMode": 500,
        |  "success": true
        |}""".stripMargin

    val response = json.as[TermsIndexResponse]

    response.correlationId shouldBe "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx"
    response.termsForService.termsForDays shouldBe empty
    response.termsForService.additionalData.isPreparationRequired shouldBe true
    response.termsForService.additionalData.preparationItems should have size 2
  }

  test("TermsIndexResponse decodes real terms/index payload") {
    val json =
      """{
        |  "correlationId": "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx",
        |  "termsForService": {
        |    "serviceVariantId": 12463,
        |    "additionalData": {
        |      "isPreparationRequired": true,
        |      "preparationItems": [
        |        { "header": "Na czym polega zabieg?", "text": "Indywidualna praca..." }
        |      ],
        |      "previousTermsAvailable": false,
        |      "nextTermsAvailable": true
        |    },
        |    "termsForDays": [],
        |    "termsInfoForDays": [
        |      {
        |        "day": "2026-01-06T00:00:00",
        |        "termsStatus": 2,
        |        "message": "W tym dniu nie realizujemy tej uslugi.",
        |        "isLimitedDay": false,
        |        "termsCounter": { "termsNumber": 0, "partialTermsCounters": [] }
        |      },
        |      {
        |        "day": "2026-01-07T00:00:00",
        |        "termsStatus": 0,
        |        "message": "W tym dniu dostepne sa terminy.",
        |        "isLimitedDay": true,
        |        "termsCounter": {
        |          "termsNumber": 22,
        |          "partialTermsCounters": [
        |            { "clinicGroupId": 2149, "doctorId": 11111, "priority": 1, "termsNumber": 1 },
        |            { "clinicGroupId": 2502, "doctorId": 22222, "priority": 1, "termsNumber": 5 }
        |          ]
        |        }
        |      }
        |    ]
        |  },
        |  "pMode": 500,
        |  "success": true
        |}""".stripMargin

    val response = json.as[TermsIndexResponse]
    response.termsForService.termsForDays shouldBe empty
    response.termsForService.additionalData.preparationItems should have size 1
  }

  test("ServiceReferralResponse decodes real GetServiceReferral payload") {
    val json =
      """{
        |  "serviceReferrals": [
        |    {
        |      "id": 999999999,
        |      "serviceVariantId": 12463,
        |      "serviceName": "KINEZYTERAPIA Terapia indywidualna - kregoslup 1 odcinek",
        |      "isOnWhitelist": false,
        |      "issued": "2025-09-15T00:00:00",
        |      "prefix": "mgr",
        |      "firstName": "DOCTOR_FIRSTNAME",
        |      "lastName": "DOCTOR_LASTNAME",
        |      "expires": "2026-03-14T00:00:00",
        |      "requiresPreparation": true,
        |      "priority": 1
        |    },
        |    {
        |      "id": 999999998,
        |      "serviceVariantId": 12455,
        |      "serviceName": "FIZYKOTERAPIA Prady tens - kregoslup 1 odcinek",
        |      "isOnWhitelist": false,
        |      "issued": "2025-09-15T00:00:00",
        |      "prefix": "mgr",
        |      "firstName": "DOCTOR_FIRSTNAME",
        |      "lastName": "DOCTOR_LASTNAME",
        |      "expires": "2026-03-14T00:00:00",
        |      "requiresPreparation": true,
        |      "priority": 2
        |    }
        |  ]
        |}""".stripMargin

    val response = json.as[ServiceReferralResponse]

    response.serviceReferrals should have size 2
    response.primaryReferral.map(_.id) shouldBe Some(999999999L)
    response.primaryReferral.map(_.serviceVariantId) shouldBe Some(12463L)
    response.primaryReferral.map(_.requiresPreparation) shouldBe Some(true)

    val second = response.serviceReferrals.sortBy(_.priority).tail.head
    second.serviceVariantId shouldBe 12455L
    second.priority shouldBe 2
  }

  test("Term decodes real oneDayTerms payload with null clinic and impedimentText") {
    // The Term is embedded in TermsForDay; we test the problematic null fields directly
    val termJson =
      """{
        |  "dateTimeFrom": "2026-01-07T07:30:00",
        |  "dateTimeTo": "2026-01-07T08:00:00",
        |  "doctor": {
        |    "id": 11111, "genderId": 1, "academicTitle": "mgr",
        |    "firstName": "DOCTOR_FIRSTNAME", "lastName": "DOCTOR_LASTNAME"
        |  },
        |  "clinicId": 2149,
        |  "clinic": null,
        |  "clinicGroupId": 2149,
        |  "clinicGroup": null,
        |  "roomId": 9058,
        |  "serviceId": 12463,
        |  "scheduleId": 16772513,
        |  "isImpediment": false,
        |  "impedimentText": null,
        |  "isAdditional": false,
        |  "isTelemedicine": false,
        |  "isInfectionTreatmentCenter": false,
        |  "partOfDay": 1,
        |  "priority": 1
        |}""".stripMargin

    val term = termJson.as[Term]

    term.clinic shouldBe None
    term.impedimentText shouldBe None
    term.clinicId shouldBe 2149L
    term.scheduleId shouldBe 16772513L
    term.roomId shouldBe 9058L
    term.doctor.id shouldBe 11111L
    term.isImpediment shouldBe false
    term.isTelemedicine shouldBe false
  }

  test("Term decodes real oneDayTerms payload with non-null clinic") {
    val termJson =
      """{
        |  "dateTimeFrom": "2026-01-07T13:30:00",
        |  "dateTimeTo": "2026-01-07T14:00:00",
        |  "doctor": {
        |    "id": 22222, "genderId": 2, "academicTitle": "mgr",
        |    "firstName": "ANOTHER_DOCTOR", "lastName": "ANOTHER_LASTNAME"
        |  },
        |  "clinicId": 2502,
        |  "clinic": "LX Warszawa - Domaniewska 52",
        |  "clinicGroupId": 2502,
        |  "clinicGroup": "ul. Domaniewska 52",
        |  "roomId": 13221,
        |  "serviceId": 12463,
        |  "scheduleId": 14020993,
        |  "isImpediment": false,
        |  "impedimentText": "",
        |  "isAdditional": false,
        |  "isTelemedicine": false,
        |  "isInfectionTreatmentCenter": false,
        |  "partOfDay": 2,
        |  "priority": 1
        |}""".stripMargin

    val term = termJson.as[Term]

    term.clinic shouldBe Some("LX Warszawa - Domaniewska 52")
    term.impedimentText shouldBe Some("")
    term.scheduleId shouldBe 14020993L
  }

  test("ReferralsResponse serialization roundtrip") {
    val procedure = RehabProcedure(name = "KINEZYTERAPIA", count = 10, preparation = None)
    val referral = Referral(
      referralId = Some(999999902L),
      eReferralId = None,
      specializationName = "Rehabilitacja",
      referralType = "Rehabilitation",
      isVisitAppointable = true,
      referralValidDate = Some("2026-12-31T00:00:00"),
      serviceInstanceId = 999999903L,
      proceduresAmount = 10,
      procedures = Some(List(procedure)),
      doctorName = Some("Dr. Smith"),
      referralIssuedDate = Some("2026-01-01T00:00:00"),
      serviceVariantId = 12463L,
      actions = Some(List(4)),
    )
    val response = ReferralsResponse(referrals = List(referral))
    val json = response.asJson
    json should include("Rehabilitacja")
    json should include("999999903")
  }

  test("ServiceReferralResponse serialization roundtrip") {
    val item = ServiceReferralItem(id = 999999902L, serviceVariantId = 12463L, serviceName = "Rehabilitacja", priority = 1)
    val response = ServiceReferralResponse(serviceReferrals = List(item))
    val json = response.asJson
    json should include("999999902")
    json should include("Rehabilitacja")

    val deserialized = json.as[ServiceReferralResponse]
    deserialized.serviceReferrals should have size 1
    deserialized.primaryReferral.map(_.id) shouldBe Some(999999902L)
    deserialized.primaryReferral.map(_.serviceVariantId) shouldBe Some(12463L)
  }

  test("RehabFacilitiesResponse serialization roundtrip") {
    val location = RehabLocation(id = 7L, name = "Warszawa")
    val facility = RehabFacility(id = 100L, name = "LX Fizjoterapia Warszawa", locationId = 7L, availabilityLevel = 1)
    val response = RehabFacilitiesResponse(locations = List(location), facilities = List(facility))
    val json = response.asJson
    json should include("Warszawa")
    json should include("LX Fizjoterapia Warszawa")

    val deserialized = json.as[RehabFacilitiesResponse]
    deserialized.locations should have size 1
    deserialized.facilities should have size 1
    deserialized.locations.head.name shouldBe "Warszawa"
    deserialized.facilities.head.locationId shouldBe 7L
  }

  test("Referral isVisitAppointable filter logic") {
    val rehab = Referral(
      referralId = Some(1L), eReferralId = None,
      specializationName = "Rehabilitacja",
      referralType = "Rehabilitation",
      isVisitAppointable = true,
      referralValidDate = None,
      serviceInstanceId = 100L,
      proceduresAmount = 5,
      procedures = None,
      doctorName = None,
      referralIssuedDate = None,
      serviceVariantId = 12463L,
      actions = None,
    )
    val other = rehab.copy(specializationName = "Kardiologia")
    val notAppointable = rehab.copy(isVisitAppointable = false)

    val referrals = List(rehab, other, notAppointable)
    val filtered = referrals.filter(r =>
      r.specializationName == "Rehabilitacja" && r.isVisitAppointable
    )
    filtered should have size 1
    filtered.head.serviceInstanceId shouldBe 100L
  }
}
