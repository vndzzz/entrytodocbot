package org.vndzzz.entrytodoc.dao

/**
  * Created by vn on 14.12.2016.
  */

import org.json4s.ShortTypeHints
import org.json4s.jackson.Serialization
import org.json4s.jackson.Serialization.{read, write}
import org.vndzzz.entrytodoc.user.{UserAction, WorkFlow}

object JsonConverter {
  implicit val formats = Serialization.formats(ShortTypeHints(List(
    classOf[UserAction.EnterDateOfBirth],
    classOf[UserAction.EnterPolisNumber],
    classOf[UserAction.EnterAlias],
    classOf[UserAction.SubmitRequest],
    WorkFlow.ShowSchedule.getClass,
    WorkFlow.NewPolisInfo.getClass,
    classOf[WorkFlow.WorkWithPolis]
  )))

  def encode[T <: AnyRef](obj: T): String = write[T](obj)

  def decode[T <: AnyRef](value: String)(implicit manifest: Manifest[T]): T = read[T](value)
}
