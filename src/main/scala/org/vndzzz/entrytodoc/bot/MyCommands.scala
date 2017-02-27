package org.vndzzz.entrytodoc.bot

import info.mukel.telegrambot4s.api.{BotBase, TelegramApiException}
import info.mukel.telegrambot4s.methods.SendMessage
import info.mukel.telegrambot4s.models.Message
import org.vndzzz.entrytodoc.GlobalImplicits
import org.vndzzz.entrytodoc.dao.{JsonConverter, Repository}

import scala.collection.mutable
import scala.concurrent.Future

/**
  * Created by vn on 03.12.2016.
  */
trait MyCommands extends BotBase with GlobalImplicits {
  type Action = Message => (Seq[String] => Future[_])
  protected val commands = mutable.HashMap[String, Action]()

  override def onMessage(message: Message): Unit = {
    Repository.putMetrics(message.chat.id, JsonConverter.encode(message), message.text.getOrElse(""))
    val f = handleMessageInt(message)
    f.fold(super.onMessage(message))(handleFuture(_, message.sender))
  }

  /**
    * handleMessage
    *
    * Parses messages and spawns bot commands accordingly.
    * Commands are case-iNsEnSiTiVe.
    *
    * Example 'command':
    * /command arg0 arg1
    */
  def handleMessageInt(message: Message): Option[Future[_]] = {
    def cleanCmd(cmd: String): String = {
      val cmdEnd = if (cmd.contains('@')) cmd.indexOf('@') else cmd.length
      cmd.substring(0, cmdEnd).toLowerCase
    }

    val accepted = for {
      text <- message.text
      Array(cmd, args @ _ *) = text.trim.split(" ")
      action <- commands.get(cleanCmd(cmd))
    } yield action(message)(args)
    accepted
  }

  def handleFuture(f: Future[_], chatId: Long) = f.failed.foreach {
    case TelegramApiException(_, 400, _, _) =>
    case e =>
      e.printStackTrace();
      request(SendMessage(Left(chatId), s"Произошла ошибка. ${Pics.Error}\nПопробуйте еще раз."))
  }

  /**
    * on
    *
    * Makes the bot able react to 'command' with the specified handler.
    * 'action' receives a message and the arguments as parameters.
    */
  def on(command: String)(action: Action): Unit = {
    commands += (command -> action)
  }

}
