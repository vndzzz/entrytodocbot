package org.vndzzz.entrytodoc.service

import com.softwaremill.tagging._
import org.json4s.JsonAST.{JField, JObject, JString}
import org.json4s.jackson.JsonMethods._
import org.json4s.{DefaultFormats, JValue}
import org.vndzzz.entrytodoc.entity._

import scala.util.{Success, Try}
import scala.xml.XML

/**
  * Created by vn on 20.12.2016.
  */
class JsonHasError(json: String) extends RuntimeException(json)

trait JsonReader[T] {
  def read(value: String): T
}

object JsonReader {
  implicit val formats = DefaultFormats

  def read[T](value: String)(implicit reader: JsonReader[T]): T = reader.read(value)

  private def checkJson(value: String) = {
    val json = parse(value)
    val success = (json \ "success").toOption.map(_.extract[Boolean])
    val hasError = (json \ "hasError").toOption.map(_.extract[Boolean])
    if (success.fold(hasError.fold(false)(identity))(!_))
      throw new JsonHasError(value)
    json
  }

  implicit object RecordsReader extends JsonReader[List[RecordResult]] {
    private def getValue[T <: JValue](values: List[(String, JValue)], valueName: String)(
        implicit manifest: Manifest[T]) = {
      values.collect { case (name, value: T) if name == valueName => value.values }.headOption
    }

    override def read(value: String): List[RecordResult] = {
      val json: JValue = checkJson(value)
      val items = json \ "items"
      for {
        JObject(oneRecord) <- items
        valueStr = (name: String) => getValue[JString](oneRecord, name).getOrElse("")
        JField("lpu", JObject(lpu)) <- oneRecord
        JField("NAME", JString(lpuName)) <- lpu
        JField("ADDRESS", JString(lpuAddress)) <- lpu
        JField("LPUCODE", JString(lpuCode)) <- lpu
        JField("@attributes", JObject(attr)) <- oneRecord
        JField("DVTID", JString(id)) <- attr
      } yield
        RecordResult(
          datetime = s"${valueStr("ShortDate")} ${valueStr("Time_from")}",
          fio = s"${valueStr("V_Family")} ${valueStr("V_Name")} ${valueStr("V_Ot")}",
          doctorData = s"${valueStr("PRVSName")} каб. ${valueStr("RoomNum")}",
          lpuName = lpuName,
          lpuAddress = lpuAddress,
          stubNum = valueStr("StubNum"),
          id = id.taggedWith[RecordId],
          lpuCode = lpuCode.taggedWith[LpuCode]
        )
    }
  }

  implicit object CancelReader extends JsonReader[Option[String]] {
    override def read(value: String): Option[String] = {
      val json = checkJson(value)
      val result = (json \ "result" \\ "CancelVisitResult").extract[String]
      val xml = XML.loadString(result) \\ "CancelVisitResult"
      if ((xml \@ "Result").toBoolean)
        None
      else
        Some((xml \ "ErrorDescription").text)
    }
  }

  implicit object LocalLpuReader extends JsonReader[Either[String, List[Lpu]]] {
    override def read(value: String): Either[String, List[Lpu]] = {
      val jsonT = Try(checkJson(value))
      val items = jsonT.map(json => (json \ "items").extract[List[Lpu]])
      items
        .transform(l => Success(Right(l)),
                   _ => Success(Left("Проверьте корректность введенных данных полиса")))
        .get
    }
  }

  implicit object LpuReader extends JsonReader[List[(String, Lpu)]] {
    override def read(value: String): List[(String, Lpu)] = {
      val json = checkJson(value)
      val JObject(list) = json \ "items"
      list.map { case (name, lpu) => (name, lpu.extract[Lpu]) }
    }
  }

  implicit object SpecialityReader extends JsonReader[List[Speciality]] {
    override def read(value: String): List[Speciality] = {
      val json = checkJson(value)
      val specialities = (json \ "items").extract[List[Speciality]]
      specialities
    }
  }

  implicit object DocPostReader extends JsonReader[DocPostResult] {
    override def read(value: String): DocPostResult = {
      val json = checkJson(value)
      val lpu = (json \ "lpu").extract[Lpu]
      val speciality = (json \ "speciality").extract[Speciality]
      val dateInfos = (json \ "dates").extract[List[DateInfo]]
      val items = (json \ "items").extract[List[PossibleVisit]]
      def totalTicketsCount(l: List[DoctorDayInfo]): Int =
        l.foldLeft(0)((s, ddi) => Try(ddi.TicketCount.toInt).getOrElse(0) + s)
      def timeFormat(value: String) = Try(value.toInt.formatted("%02d")).getOrElse("00")
      val values: Map[String, (List[DoctorDayInfo], DocPost)] = items
        .map(pv => {
          val doctorDayInfos = for {
            d <- pv.DayScheduleList.DaySchedul
            if d.FlagAccess != 3 && d.FlagAccess != 0
            di <- dateInfos
            if di.rawDate == d.ProcessedDate
          } yield
            DoctorDayInfo(d.ProcessedDate,
                          s"${timeFormat(d.HourFrom)}:${timeFormat(d.MinuteFrom)}",
                          s"${timeFormat(d.HourTo)}:${timeFormat(d.MinuteTo)}",
                          d.TicketCount,
                          di.text,
                          di.date)
          (pv.DocPost.ID, (doctorDayInfos, pv.DocPost))
        })
        .filter(_._2._1.nonEmpty)
        .toMap
        .map {
          case (key, (l, docPost)) => (key, (l, docPost.copy(TicketCount = totalTicketsCount(l))))
        }
      DocPostResult(speciality,
                    lpu,
                    values.values.map(_._2).toList,
                    values.mapValues(_._1))
    }
  }

  implicit object CityReader extends JsonReader[List[City]] {
    override def read(value: String): List[City] = {
      val json = checkJson(value)
      (json \ "items").extract[List[City]]
    }
  }

  implicit object LastStepReader extends JsonReader[Unit] {
    override def read(value: String) = {
      checkJson(value)
    }
  }

  implicit object CreateVisitReader extends JsonReader[Either[String, String]] {
    override def read(value: String) = {
      val json = checkJson(value)
      val items = (json \ "items" \\ "CreateVisitResult").extract[String]
      val xml = XML.loadString(items) \\ "CreateVisitResult"
      val stubNum = xml \@ "StubNum"
      val isValid = (xml \@ "Result").toBoolean
      val error = (xml \ "ErrorDescription").text
      if (isValid)
        Right(stubNum)
      else
        Left(error)
    }
  }

  implicit object ScheduleReader extends JsonReader[List[TimeItem]] {
    override def read(value: String) = {
      val json = checkJson(value)
      val outDate = (json \ "date").extract[String]
      val items = (json \ "timeItems").extract[List[TimeItem]]
      items.filter(x => x.attrs.BusyFlag == "0" && (x.attrs.FlagAccess != "3"))
    }
  }

}
