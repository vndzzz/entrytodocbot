package org.vndzzz.entrytodoc.entity

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

/**
  * Created by vn on 28.12.2016.
  */

case class DayInfo(Date: String, HourFrom: String, HourTo: String, MinuteFrom: String, MinuteTo: String, TicketCount: String, FlagAccess: Int) {
  val ProcessedDate: String = LocalDateTime.parse(Date, DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss")).format(DateTimeFormatter.ISO_DATE)
}
