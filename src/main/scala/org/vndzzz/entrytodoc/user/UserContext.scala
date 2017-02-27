package org.vndzzz.entrytodoc.user

import com.softwaremill.tagging.@@
import io.getquill.Embedded
import org.vndzzz.entrytodoc.dao.JsonConverter
import org.vndzzz.entrytodoc.entity._

/**
  * Created by vn on 19.11.2016.
  */
case class UserContext (action: UserAction = UserAction.Empty, flow: WorkFlow = WorkFlow.Empty, selectedItem: SelectedItem = SelectedItem()) {
  def toJsonString = JsonConverter.encode(this)
}
object UserContext {
  def fromJsonString(value: String) = JsonConverter.decode[UserContext](value)
}

case class PolisInfo(id: Long = 0, alias: String = "", polis: String="", dateOfBirth: String = "") extends Embedded

sealed trait UserAction
object UserAction {
  final case object Empty extends UserAction
  final case class EnterPolisNumber(polisInfo: Option[PolisInfo] = None) extends UserAction
  final case class EnterDateOfBirth(polisInfo: PolisInfo) extends UserAction
  final case class EnterAlias(polisInfo: PolisInfo) extends UserAction
  final case class SubmitRequest(polisInfo: PolisInfo) extends UserAction
}

sealed trait WorkFlow
object WorkFlow {
  final case object Empty extends WorkFlow
  final case object ShowSchedule extends WorkFlow
  final case object NewPolisInfo  extends WorkFlow
  final case class WorkWithPolis(polisInfo: PolisInfo) extends WorkFlow
}

case class SelectedItem(cityId: Option[String@@CityId] = None,
                        lpuCode: Option[String@@LpuCode] = None,
                        specId: Option[String@@SpecialityId] = None,
                        postId: Option[String@@DocPostId] = None,
                        date: Option[String] = None,
                        time: Option[String] = None,
                        posId: Option[String@@TimeItemId] = None)
