package com.lbs.api.json.model

case class ReferralsResponse(
  referrals: List[Referral]
) extends SerializableJsonObject

case class Referral(
  referralId: Option[Long],
  eReferralId: Option[Long],
  specializationName: String,
  referralType: String,
  isVisitAppointable: Boolean,
  referralValidDate: Option[String],
  serviceInstanceId: Long,
  proceduresAmount: Int,
  procedures: Option[List[RehabProcedure]],
  doctorName: Option[String],
  referralIssuedDate: Option[String],
  serviceVariantId: Long,
  actions: Option[List[Int]],
) extends SerializableJsonObject

case class RehabProcedure(
  name: String,
  count: Int,
  preparation: Option[String]
) extends SerializableJsonObject
