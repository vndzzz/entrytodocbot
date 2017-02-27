package org.vndzzz.entrytodoc.entity

import com.softwaremill.tagging.@@

/**
  * Created by vn on 28.12.2016.
  */
trait DoctorId
case class Doctor(ID: String@@DoctorId, Family: String, Name: String, Patronymic: String)
case class DayScheduleList(DaySchedul: List[DayInfo])

