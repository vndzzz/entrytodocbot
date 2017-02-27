package org.vndzzz.entrytodoc.entity

import com.softwaremill.tagging.@@

/**
  * Created by vn on 28.12.2016.
  */
trait DocPostId
case class DocPost(ID: String@@DocPostId, Doctor: Doctor, Room: String, Post: String, Sepatation: String, TicketCount: Int = 0)
