package org.vndzzz.entrytodoc.bot

/**
  * Created by vn on 21.11.2016.
  */
sealed trait Command {
  val name: String
  val args: String
}

object Command {

  case object TimePlan extends Command {
    val name = Pics.Calendar
    val args = "График работы"
  }

  case object PolisList extends Command {
    val name = Pics.PolisList
    val args = "Список полисов"
  }

  case object MainMenu extends Command {
    val name = Pics.MenuUp
    val args = "Главное меню"
  }

  case object MyRecords extends Command {
    val name = Pics.RecordList
    val args = "Мои записи"
  }

  case object MakeRecord extends Command {
    val name = Pics.Record
    val args = "Записаться"
  }

  case object Rate extends Command {
    val name = Pics.Rate
    val args = "Оценить бота"
  }


  implicit def commandToString(command: Command): String = s"${command.name} ${command.args}"
}




