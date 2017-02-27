package org.vndzzz.entrytodoc.bot

import com.softwaremill.tagging.{@@, _}
import org.vndzzz.entrytodoc.entity._

import scala.util.matching.Regex

/**
  * Created by vn on 25.11.2016.
  */
sealed trait CallbackCommand {
  def formatRegex(args: Any*): Regex = {
    ("^" + pattern + "$").format(args: _*).r
  }

  protected def pattern: String
}

object CallbackCommand {

  case object CityOrder extends CallbackCommand {
    override protected val pattern = "cityorder=%s"

    def apply(o: Int): String = pattern.format(o)

    def unapply(arg: Option[String]): Option[Int] = {
      val r = formatRegex("(\\d+)")
      arg collect { case r(o) => o.toInt }
    }
  }

  case object CityAll extends CallbackCommand {
    override protected val pattern = "cityall"

    def apply(x: Unit) = pattern

    def unapply(arg: Option[String]): Boolean = arg forall (_ == pattern)
  }

  case object City extends CallbackCommand {
    def apply(id: String @@ CityId) = pattern.format(id)

    override protected def pattern = "city=%s"

    def unapply(arg: Option[String]): Option[String @@ CityId] = {
      val r = formatRegex("(.+)")
      arg.collect { case r(id) => id.taggedWith[CityId] }
    }
  }

  case object LpuOrder extends CallbackCommand {
    def apply(order: Int, cityId: String @@ CityId) =
      pattern.format(order, cityId)

    override protected def pattern: String = "lpuorder=%s;cityid=%s"

    def unapply(arg: Option[String]): Option[(Int, String @@ CityId)] = {
      val r = formatRegex("(\\d+)", "(.+)")
      arg collect {
        case r(order, cityId) => (order.toInt, cityId.taggedWith[CityId])
      }
    }
  }

  case object LpuAll extends CallbackCommand {
    override protected def pattern: String = "lpuall;cityid=%s"

    def apply(cityId: String @@ CityId) = pattern.format(cityId)

    def unapply(arg: Option[String]): Option[String @@ CityId] = {
      val r = formatRegex("(.+)")
      arg collect { case r(cityId) => cityId.taggedWith[CityId] }
    }
  }

  case object Lpu extends CallbackCommand {
    def apply(id: String @@ LpuCode) = pattern.format(id)

    override protected def pattern: String = "lpu=%s"

    def unapply(arg: Option[String]): Option[String @@ LpuCode] = {
      val r = formatRegex("(.+)")
      arg collect { case r(id) => id.taggedWith[LpuCode] }
    }
  }

  case object Speciality extends CallbackCommand {
    def apply(specId: String @@ SpecialityId, lpuCode: String @@ LpuCode) =
      pattern.format(specId, lpuCode)

    override protected def pattern = "spec=%s;lpu=%s"

    def unapply(arg: Option[String]): Option[(String @@ SpecialityId, String @@ LpuCode)] = {
      val r = formatRegex("(.+)", "(.+)")
      arg collect {
        case r(specId, lpuId) =>
          (specId.taggedWith[SpecialityId], lpuId.taggedWith[LpuCode])
      }
    }
  }

  case object Doctors extends CallbackCommand {
    override protected def pattern: String = "docpost=%s;lpuid=%s"

    def apply(docPostId: String @@ DocPostId, lpuCode: String @@ LpuCode) =
      pattern.format(docPostId, lpuCode)

    def unapply(arg: Option[String]): Option[(String @@ DocPostId, String @@ LpuCode)] = {
      val r = formatRegex("(.+)", "(.+)")
      arg collect {
        case r(docPostId, lpuCode) =>
          (docPostId.taggedWith[DocPostId], lpuCode.taggedWith[LpuCode])
      }
    }
  }

  case object Schedule extends CallbackCommand {
    override protected def pattern: String = "lpuId=%s;docpost=%s;date=%s"

    def apply(lpuCode: String @@ LpuCode, docPostId: String @@ DocPostId, date: String) =
      pattern.format(lpuCode, docPostId, date)

    def unapply(arg: Some[String]): Option[(String @@ LpuCode, String @@ DocPostId, String)] = {
      val r = formatRegex("(.+)", "(.+)", "(.+)")
      arg collect {
        case r(lpuCode, docPostId, date) =>
          (lpuCode.taggedWith[LpuCode], docPostId.taggedWith[DocPostId], date)
      }
    }
  }

  case object Record extends CallbackCommand {
    override protected def pattern: String = "l=%s;d=%s;dt=%s;p=%s;t=%s"

    def apply(lpuCode: String @@ LpuCode,
              docPostId: String @@ DocPostId,
              date: String,
              posId: String @@ TimeItemId,
              time: String) =
      pattern.format(lpuCode, docPostId, date, posId, time)

    def unapply(arg: Some[String])
      : Option[(String @@ LpuCode, String @@ DocPostId, String, String @@ TimeItemId, String)] = {
      val r = formatRegex("(.+)", "(.+)", "(.+)", "(.+)", "(.+)")
      arg collect {
        case r(lpuId, docPostId, date, posId, time) =>
          (lpuId.taggedWith[LpuCode],
           docPostId.taggedWith[DocPostId],
           date,
           posId.taggedWith[TimeItemId],
           time)
      }
    }
  }

  case object RecordDo extends CallbackCommand {
    override protected def pattern = "c;l=%s;d=%s;dt=%s;p=%s;t=%s"

    def apply(lpuId: String @@ LpuCode,
              docPostId: String @@ DocPostId,
              date: String,
              posId: String @@ TimeItemId,
              time: String) =
      pattern.format(lpuId, docPostId, date, posId, time)

    def unapply(arg: Some[String])
      : Option[(String @@ LpuCode, String @@ DocPostId, String, String @@ TimeItemId, String)] = {
      val r = formatRegex("(.+)", "(.+)", "(.+)", "(.+)", "(.+)")
      arg collect {
        case r(lpuId, docPostId, date, posId, time) =>
          (lpuId.taggedWith[LpuCode],
           docPostId.taggedWith[DocPostId],
           date,
           posId.taggedWith[TimeItemId],
           time)
      }
    }
  }

  case object ViewPolis extends CallbackCommand {
    override def pattern = "pv=%s"

    def apply(polisKey: Long) = pattern.format(polisKey)

    def unapply(arg: Some[String]): Option[Long] = {
      val r = formatRegex("(\\d+)")
      arg collect { case r(polisKey) => polisKey.toLong }
    }
  }

  case object DeletePolis extends CallbackCommand {
    override def pattern = "pd=%s"

    def apply(polisKey: Long) = pattern.format(polisKey)

    def unapply(arg: Some[String]): Option[Long] = {
      val r = formatRegex("(\\d+)")
      arg collect { case r(polisKey) => polisKey.toLong }
    }
  }

  case object AddPolis extends CallbackCommand {
    override def pattern = "addpolis"

    def apply(x: Unit) = pattern

    def unapply(arg: Some[String]): Boolean = arg.getOrElse("") == pattern
  }

  case object RecordPolis extends CallbackCommand {
    override def pattern = "pr=%s"

    def apply(polisId: Long) = pattern.format(polisId)

    def unapply(arg: Some[String]): Option[Long] = {
      val r = formatRegex(("(\\d+)"))
      arg collect { case r(polis) => polis.toLong }
    }
  }

  case object EnterPolis extends CallbackCommand {
    override def pattern = "enterpolismanual"

    def apply(x: Unit) = pattern

    def unapply(arg: Some[String]): Boolean = arg.getOrElse("") == pattern
  }

  case object CancelRecord extends CallbackCommand {
    override def pattern = "cr;id=%s;lpu=%s"
    def apply(recordId: String @@ RecordId, lpuCode: String @@ LpuCode) =
      pattern.format(recordId, lpuCode)

    def unapply(arg: Some[String]): Option[(String @@ RecordId, String @@ LpuCode)] = {
      val r = formatRegex("(.+)", "(.+)")
      arg collect {
        case r(recordId, lpuCode) =>
          (recordId.taggedWith[RecordId], lpuCode.taggedWith[LpuCode])
      }
    }

  }

  case object CancelBack extends CallbackCommand {
    override def pattern = "cb;id=%s;lpu=%s"
    def apply(recordId: String @@ RecordId, lpuCode: String @@ LpuCode) =
      pattern.format(recordId, lpuCode)

    def unapply(arg: Some[String]): Option[(String @@ RecordId, String @@ LpuCode)] = {
      val r = formatRegex("(.+)", "(.+)")
      arg collect {
        case r(recordId, lpuCode) =>
          (recordId.taggedWith[RecordId], lpuCode.taggedWith[LpuCode])
      }
    }
  }

  case object CancelConfirm extends CallbackCommand {
    override def pattern = "cc;id=%s;lpu=%s"
    def apply(recordId: String @@ RecordId, lpuCode: String @@ LpuCode) =
      pattern.format(recordId, lpuCode)

    def unapply(arg: Some[String]): Option[(String @@ RecordId, String @@ LpuCode)] = {
      val r = formatRegex("(.+)", "(.+)")
      arg collect {
        case r(recordId, lpuCode) =>
          (recordId.taggedWith[RecordId], lpuCode.taggedWith[LpuCode])
      }
    }
  }
}
