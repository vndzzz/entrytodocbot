package org.vndzzz.entrytodoc.bot

import info.mukel.telegrambot4s.models.{KeyboardButton, ReplyKeyboardMarkup}
import org.vndzzz.entrytodoc.bot.Command._

/**
  * Created by vn on 19.12.16.
  */
object ReplyMarkups {
  implicit def commandToKeyboardButton(command: Command): KeyboardButton = KeyboardButton(command)
  val MainMenu = ReplyKeyboardMarkup(Seq(Seq(PolisList, TimePlan), Seq(Rate)), Some(true), None, None)
  val PolisSelected = ReplyKeyboardMarkup(Seq(Seq(MyRecords, MakeRecord), Seq(PolisList, Command.MainMenu)), Some(true), Some(false), Some(true))
}
