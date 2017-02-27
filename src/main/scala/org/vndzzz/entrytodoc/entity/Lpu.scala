package org.vndzzz.entrytodoc.entity

import com.softwaremill.tagging.@@

/**
  * Created by vn on 28.12.2016.
  */
trait LpuId
trait LpuCode
case class Lpu(ID: String@@LpuId, NAME: String, LPUCODE: String@@LpuCode, ADDRESS: String, order: Option[Int], CITY: String @@ CityId)
