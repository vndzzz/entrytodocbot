package org.vndzzz.entrytodoc.entity

import com.softwaremill.tagging.@@

/**
  * Created by vn on 11.06.2016.
  */
trait CityId
case class CityList(items: List[City], success: Boolean)
case class City(ID: String@@CityId, NAME: String, count: Int, order: Option[Int])









