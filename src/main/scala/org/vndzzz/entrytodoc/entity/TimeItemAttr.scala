package org.vndzzz.entrytodoc.entity

import com.softwaremill.tagging.@@

/**
  * Created by vn on 28.12.2016.
  */
trait TimeItemId
case class TimeItemAttr(PosID: String@@TimeItemId, BusyFlag: String, FlagAccess: String)
