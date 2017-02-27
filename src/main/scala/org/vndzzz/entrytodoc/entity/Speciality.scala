package org.vndzzz.entrytodoc.entity

import com.softwaremill.tagging.@@

/**
  * Created by vn on 28.12.2016.
  */
trait SpecialityId

case class SpecialityDesc(Code: String, Name: String, ID: String@@SpecialityId)
case class Speciality(Specialty: SpecialityDesc, Volume: Integer)
