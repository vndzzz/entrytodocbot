package org.vndzzz.entrytodoc

import org.scalatest.{FunSpec, Matchers}
import org.vndzzz.entrytodoc.entity.{LpuCode, RecordId, RecordResult}
import org.vndzzz.entrytodoc.service.JsonHasError

import scala.io.Source
import com.softwaremill.tagging._

/**
  * Created by vn on 20.12.2016.
  */
class JsonReaderSpec extends FunSpec with Matchers {

  import org.vndzzz.entrytodoc.service.JsonReader._

  def source(fileName: String): String = {
    val inputStream = getClass.getResourceAsStream("/" + fileName)
    Source.fromInputStream(inputStream).mkString
  }

  describe("read record list") {
    it("Reads json string with single element") {
      val string = source("records/myrecords_response.json")
      val resultList: List[RecordResult] = read[List[RecordResult]](string)
      resultList should have size 1
      val result = resultList.head
      val record = RecordResult(
        datetime = "30.12.2016 11:50",
        fio = "Любимова Елена Валентиновна",
        doctorData = "Педиатрия каб. 201",
        lpuName = "Детская поликлиника (ГБУЗ МО ДЦГБ)",
        lpuAddress = "Московская обл., г Домодедово, мкр Центральный, ш Каширское, дом 36",
        stubNum = "ЛЕ018",
        id = "1339663".taggedWith[RecordId],
        lpuCode = "0601012".taggedWith[LpuCode]
      )
      result should equal(record)
    }
    it("raise an error when json hasError") {
      val string = source("records/myrecords_response_error.json")
      an[JsonHasError] should be thrownBy read[List[RecordResult]](string)
    }

    it("has two elements when json has two elements") {
      val string = source("records/myrecords_response_twoelems.json")
      val resultList: List[RecordResult] = read[List[RecordResult]](string)
      resultList should have size 2

      val firstElem = RecordResult(
        datetime = "29.12.2016 09:30",
        fio = "Сайидахмедов Вадим Алиханович",
        doctorData = "Общая врачебная практика(ОВП) каб. ",
        lpuName = "Офис ВОП на Ломоносова (ГБУЗ МО ДЦГБ)",
        lpuAddress = "Московская обл., г Домодедово, мкр Северный, ул Ломоносова, дом 14",
        stubNum = "СВ007",
        id = "1353976".taggedWith[RecordId],
        lpuCode = "0601019".taggedWith[LpuCode]
      )

      val secondElem = RecordResult(
        datetime = "09.01.2017 09:15",
        fio = "Богачева Лилия Васильевна",
        doctorData = "Терапия каб. 315",
        lpuName = "Городская поликлиника (ГБУЗ МО ДЦГБ)",
        lpuAddress = "Московская обл., г Домодедово, мкр Центральный, ул Пирогова, дом 9",
        stubNum = "БЛ006",
        id = "1353974".taggedWith[RecordId],
        lpuCode = "0601011".taggedWith[LpuCode]
      )

      resultList.head should equal(firstElem)
      resultList(1) should equal(secondElem)
    }
  }

  describe("read cancel record"){
    it("should produce error from faield json") {
      val string = source("cancelrecord/cancel_error.json")
      val result = read[Option[String]](string)
      println(result)
      result should equal(Some("Указанной записи не существует"))
    }

  }

}
