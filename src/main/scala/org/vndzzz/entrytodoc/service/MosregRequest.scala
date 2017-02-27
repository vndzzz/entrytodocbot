package org.vndzzz.entrytodoc.service

import java.time.format.DateTimeFormatter

import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers._
import com.softwaremill.tagging.@@
import org.vndzzz.entrytodoc.entity._
import org.vndzzz.entrytodoc.user.PolisInfo

import scala.collection.immutable.Seq

/**
  * Created by vn on 20.12.2016.
  */
sealed trait MosregRequest[T] {
  type Response = T

  def httpRequest: HttpRequest
}

object MosregRequest {
  private val commonHeaders: Seq[HttpHeader] = Seq(
    Host("uslugi.mosreg.ru"),
    Origin(HttpOrigin("https://uslugi.mosreg.ru")))
  private val commonHeadersWithCookie: (Cookie) => Seq[HttpHeader] =
    (cookie: Cookie) => commonHeaders :+ cookie

  final case class LocalLpuRequest(polisInfo: PolisInfo)
      extends MosregRequest[Either[String, List[Lpu]]] {
    override def httpRequest: HttpRequest = {
      val uri = Uri("https://uslugi.mosreg.ru/zdrav/doctor_appointment/submit")
      val form = FormData(
        Map(
          "sPol" -> "",
          "nPol" -> polisInfo.polis,
          "birthday" -> polisInfo.dateOfBirth.format(
            DateTimeFormatter.ofPattern("dd.MM.yyyy"))
        ))
      HttpRequest(HttpMethods.POST, uri, commonHeaders, form.toEntity)
    }
  }

  final case class Records(cookie: Cookie)
      extends MosregRequest[List[RecordResult]] {
    override def httpRequest: HttpRequest = {
      val uri = Uri(
        "https://uslugi.mosreg.ru/zdrav/doctor_appointment/patient_orders")
      val referer = Referer(Uri("https://uslugi.mosreg.ru/zdrav/"))
      HttpRequest(HttpMethods.GET,
                  uri,
                  commonHeadersWithCookie(cookie) :+ referer)
    }

  }

  final case class Cancel(cookie: Cookie,
                          lpuCode: String @@ LpuCode,
                          id: String @@ RecordId)
      extends MosregRequest[Option[String]] {
    override def httpRequest: HttpRequest = {
      val uri = Uri(
        "https://uslugi.mosreg.ru/zdrav/doctor_appointment/cancel_visit")
      val referer = Referer(Uri("https://uslugi.mosreg.ru/zdrav/"))
      val form = FormData(
        Map(
          "lpuCode" -> lpuCode,
          "visitId" -> id
        ))
      HttpRequest(HttpMethods.POST,
                  uri,
                  commonHeadersWithCookie(cookie) :+ referer,
                  form.toEntity)
    }
  }

  final case class LpuListRequest(cityId: String @@ CityId)
      extends MosregRequest[List[(String, Lpu)]] {
    override def httpRequest =
      HttpRequest(uri = Uri(
        s"https://uslugi.mosreg.ru/zdrav/doctor_appointment/lpu_list/$cityId"))
  }

  final case class SpecialityRequest(lpuCode: String @@ LpuCode)
      extends MosregRequest[List[Speciality]] {
    override def httpRequest =
      HttpRequest(uri = Uri(
        s"https://uslugi.mosreg.ru/zdrav/doctor_appointment/lpu?lpuCode=$lpuCode&scenery=1"))
  }

  final case class DocPostRequest(lpuId: String @@ LpuCode,
                                  specId: String @@ SpecialityId)
      extends MosregRequest[DocPostResult] {
    override def httpRequest =
      HttpRequest(uri = Uri(
        s"https://uslugi.mosreg.ru/zdrav/doctor_appointment/doctors_list?lpuCode=$lpuId&specId=$specId&days=14&scenery=1"))
  }

  final case object CityRequest extends MosregRequest[List[City]] {
    val httpRequest = HttpRequest(
      uri = Uri("https://uslugi.mosreg.ru/zdrav/doctor_appointment/city_list"))
  }

  final case class LastStepRequest(cityId: String @@ CityId,
                                   lpuId: String @@ LpuCode,
                                   specId: String @@ SpecialityId,
                                   postId: String @@ DocPostId,
                                   date: String,
                                   posId: String @@ TimeItemId,
                                   cookie: Cookie)
      extends MosregRequest[Unit] {
    override def httpRequest: HttpRequest = {
      val uri = Uri(
        s"https://uslugi.mosreg.ru/zdrav/doctor_appointment/set_last_step")
      val referer = Referer(Uri(
        s"https://uslugi.mosreg.ru/zdrav/?popup=alternative-appointment&city=$cityId&lpucode=$lpuId&specid=$specId&postid=$postId&date=$date"))
      val form = FormData(
        Map("scenery" -> "1", "lpuCode" -> lpuId, "DTTID" -> posId))
      HttpRequest(HttpMethods.POST,
                  uri,
                  commonHeadersWithCookie(cookie) :+ referer,
                  form.toEntity)
    }
  }

  final case class SubmitRequest(polisInfo: PolisInfo,
                                 cityId: String @@ CityId,
                                 lpuId: String @@ LpuCode,
                                 specId: String @@ SpecialityId,
                                 postId: String @@ DocPostId,
                                 date: String,
                                 posId: String @@ TimeItemId,
                                 cookie: Cookie)
      extends MosregRequest[Unit] {
    override def httpRequest: HttpRequest = {
      val uri = Uri(
        s"https://uslugi.mosreg.ru/zdrav/doctor_appointment/submit")
      val referer = Referer(Uri(
        s"https://uslugi.mosreg.ru/zdrav/?popup=alternative-appointment&city=$cityId&lpucode=$lpuId&specid=$specId&postid=$postId&date=$date&posid=$posId"))
      val form = FormData(
        Map(
          "sPol" -> "",
          "nPol" -> polisInfo.polis,
          "birthday" -> polisInfo.dateOfBirth.format(
            DateTimeFormatter.ofPattern("dd.MM.yyyy"))
        ))
      HttpRequest(HttpMethods.POST,
                  uri,
                  commonHeadersWithCookie(cookie) :+ referer,
                  form.toEntity)
    }
  }

  final case class CreateVisitRequest(polisInfo: PolisInfo,
                                      cityId: String @@ CityId,
                                      lpuId: String @@ LpuCode,
                                      specId: String @@ SpecialityId,
                                      postId: String @@ DocPostId,
                                      date: String,
                                      posId: String @@ TimeItemId,
                                      cookie: Cookie)
      extends MosregRequest[Either[String, String]] {
    override def httpRequest: HttpRequest = {
      val uri = Uri(
        s"https://uslugi.mosreg.ru/zdrav/doctor_appointment/create_visit")
      val referer = Referer(Uri(
        s"https://uslugi.mosreg.ru/zdrav/?popup=alternative-appointment&city=$cityId&lpucode=$lpuId&specid=$specId&postid=$postId&date=$date&posid=$posId"))
      val form = FormData(
        Map("scenery" -> "1", "lpuCode" -> lpuId, "DTTID" -> posId))
      HttpRequest(HttpMethods.POST,
                  uri,
                  commonHeadersWithCookie(cookie) :+ referer,
                  form.toEntity)
    }
  }

  final case class ScheduleRequest(lpuId: String @@ LpuCode,
                                   doctorPostId: String @@ DocPostId,
                                   date: String)
      extends MosregRequest[List[TimeItem]] {
    override def httpRequest =
      HttpRequest(uri = Uri(
        s"https://uslugi.mosreg.ru/zdrav/doctor_appointment/doctor/$lpuId/$doctorPostId/$date?scenery=1"))
  }

}
