package org.vndzzz.entrytodoc.entity

/**
  * Created by vn on 28.12.2016.
  */
case class DocPostResult(speciality: Speciality, lpu: Lpu, docPostList: List[DocPost], docDates: Map[String, List[DoctorDayInfo]])
