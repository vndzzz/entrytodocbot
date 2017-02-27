package org.vndzzz.entrytodoc

import org.json4s.DefaultFormats

import scala.concurrent.ExecutionContext

/**
  * Created by vn on 23.11.2016.
  */
trait GlobalImplicits {
  implicit def formats: DefaultFormats.type = DefaultFormats
  implicit def executionContext: ExecutionContext = concurrent.ExecutionContext.global
  val rateUrl = "https://telegram.me/storebot?start=entrytodoctorbot"
}
