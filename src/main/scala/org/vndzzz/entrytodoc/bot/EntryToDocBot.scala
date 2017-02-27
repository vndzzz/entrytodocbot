package org.vndzzz.entrytodoc.bot

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import com.softwaremill.quicklens._
import com.softwaremill.tagging.{@@, _}
import com.typesafe.config.ConfigFactory
import info.mukel.telegrambot4s.Implicits._
import info.mukel.telegrambot4s.api._
import info.mukel.telegrambot4s.methods.{ChatAction, SendChatAction, SendMessage, _}
import info.mukel.telegrambot4s.models._
import org.vndzzz.entrytodoc.bot.CallbackCommand._
import org.vndzzz.entrytodoc.bot.Command.{MainMenu, MakeRecord, MyRecords, PolisList, Rate, TimePlan}
import org.vndzzz.entrytodoc.dao.{Repository, UserContextDB}
import org.vndzzz.entrytodoc.entity._
import org.vndzzz.entrytodoc.user.UserAction.{Empty, EnterAlias, EnterDateOfBirth, EnterPolisNumber}
import org.vndzzz.entrytodoc.user.WorkFlow.{NewPolisInfo, ShowSchedule, WorkWithPolis}
import org.vndzzz.entrytodoc.user.{PolisInfo, UserContext, WorkFlow}

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

/**
  * Created by vn on 04.06.2016.
  */
object EntryToDocBot extends TelegramBot with Polling with MyCommands {

  lazy val config =
    if (System.getProperty("mode") == "prod")
      ConfigFactory.load("prod.conf")
    else
      ConfigFactory.load("dev.conf")

  lazy val token = config.getString("bot.token")

  on("/start") { implicit message => _ =>
    val insertContext =
      UserContextDB.insertContext(message.chat.id, UserContext())
    insertContext.flatMap { _ =>
      request(
        SendMessage(
          message.chat.id,
          s"Здравствуйте, ${message.chat.firstName
            .getOrElse("")} ${message.chat.lastName.getOrElse("")}!  \uD83D\uDE42",
          replyMarkup = ReplyMarkups.MainMenu
        ))
    }
  }

  on(TimePlan.name) { implicit message => args =>
    val f = if (args == TimePlan.args.split(" ").toSeq) {
      for {
        _ <- request(SendChatAction(message.chat.id, ChatAction.Typing))
        r = showCityList(1, message.chat.id, None)
      } yield r
    } else Future(())
    f
  }

  on(PolisList.name) { implicit message => args =>
    val f: Future[_] = if (args == PolisList.args.split(" ").toSeq) for {
      polisList <- Repository.getPolises(message.chat.id)
      buttons = polisListButtons(polisList)
    } yield request(SendMessage(message.chat.id, "Выберите сохраненный полис или добавьте новый", ParseMode.HTML, replyMarkup = InlineKeyboardMarkup(buttons)))
    else Future {}
    f
  }

  on(MainMenu.name) { implicit message => args =>
    if (args == MainMenu.args.split(" ").toSeq) {
      val chatId = message.chat.id
      for {
        uc <- Repository.getUserContext(chatId)
        _ <- Repository.updateUserContext(chatId, uc.copy(flow = WorkFlow.Empty))
      } yield
        request(SendMessage(chatId, "Вы в главном меню", replyMarkup = ReplyMarkups.MainMenu))
    } else Future.successful(())
  }

  on(MyRecords.name) { implicit message => args =>
    if (args == MyRecords.args.split(" ").toSeq) {
      val chatId = message.chat.id
      showRecords(chatId)
    } else Future.successful(())
  }

  on(MakeRecord.name) { implicit message => args =>
    if (args == MakeRecord.args.split(" ").toSeq) {
      val chatId = message.chat.id
      showLpuList(Some(1), chatId, None, " ".taggedWith[CityId])
    } else Future.successful(())
  }

  def showRateLink(chatId: Long) = {
    request(
      SendMessage(chatId,
                  text = s"""<a href="$rateUrl">Оценить бота и/или оставить отзыв</a>""",
                  parseMode = ParseMode.HTML))
  }

  on(Rate.name) { implicit message => args =>
    if (args == Rate.args.split(" ").toSeq) {
      val chatId = message.chat.id
      showRateLink(chatId)
    } else Future.successful(())
  }

  override def onCallbackQuery(callbackQuery: CallbackQuery): Unit = {
    val chatId = callbackQuery.message.get.chat.id
    request(SendChatAction(callbackQuery.message.map(_.chat.id).get, ChatAction.Typing))
    val messageId = callbackQuery.message.map(_.messageId).get
    val resultF: Future[_] = callbackQuery.data match {
      case CityOrder(order) =>
        showCityList(order, chatId, Some(messageId))
      case CityAll() =>
        showCityList(None, chatId, Some(messageId))
      case CallbackCommand.City(id) => citySelected(chatId, messageId, id)
      case LpuOrder(order, cityId) =>
        showLpuList(order, chatId, Some(messageId), cityId)
      case LpuAll(cityId) => showLpuList(None, chatId, Some(messageId), cityId)
      case CallbackCommand.Lpu(id) => lpuSelected(chatId, id, callbackQuery.id, messageId)
      case CallbackCommand.Speciality(specId, lpuId) =>
        specialitySelected(specId, lpuId, chatId, callbackQuery.id, messageId)

      case CallbackCommand.Doctors(docPostId, lpuId) =>
        showScheduleDays(chatId, docPostId, lpuId, messageId)
      case CallbackCommand.Schedule(lpuId, docPostId, date) =>
        showSchedule(lpuId, docPostId, date, chatId, messageId)
      case CallbackCommand.Record(lpuCode, docPostId, date, posId, time) =>
        scheduleSelected(lpuCode, docPostId, date, posId, time, chatId, messageId)
      case CallbackCommand.RecordDo(lpuId, docPostId, date, posId, time) =>
        recordDo(lpuId, docPostId, date, posId, time, chatId, messageId)

      case CallbackCommand.EnterPolis() => askPolisNumber(chatId)
      case CallbackCommand.RecordPolis(polisId) =>
        Repository.getPolis(chatId, polisId).flatMap(makeRecord(chatId, _))
      case CallbackCommand.AddPolis() => addPolis(chatId)
      case CallbackCommand.DeletePolis(polis) =>
        deletePolis(chatId, messageId, polis)
      case CallbackCommand.ViewPolis(polis) => viewPolis(chatId, polis)

      case CallbackCommand.CancelRecord(recordId, lpuCode) =>
        cancelShowConfirm(messageId, chatId, lpuCode, recordId)
      case CallbackCommand.CancelBack(recordId, lpuCode) =>
        cancelBack(messageId, chatId, lpuCode, recordId)
      case CallbackCommand.CancelConfirm(recordId, lpuCode) =>
        cancelRecord(messageId, chatId, lpuCode, recordId)
      case _ =>
        Future.failed(new IllegalArgumentException("Invalid callback command"))
    }
    handleFuture(resultF, chatId)
    //Repository.putMetrics(chatId, "", )
  }

  def showScheduleDays(chatId: Long,
                       docPostId: String @@ DocPostId,
                       lpuCode: String @@ LpuCode,
                       messageId: Long) = {
    def doctorDayButton(doctorDayInfo: DoctorDayInfo) =
      InlineKeyboardButton(
        s"${doctorDayInfo.dayOfWeek} ${doctorDayInfo.longDate}\n(${doctorDayInfo.TicketCount})",
        callbackData = CallbackCommand.Schedule(lpuCode, docPostId, doctorDayInfo.date)
      )
    val doctorDatesF = Repository.getDoctorDates(chatId, docPostId)
    val selectedItemF = Repository.getUserContext(chatId).map(_.selectedItem)
    for {
      selectedItem <- selectedItemF
      doctorPost <- Repository
        .doctors(selectedItem.specId.get, lpuCode)
        .map(_.find(_.ID == docPostId).get)
      text = doctorText(doctorPost)
      l <- doctorDatesF
      text2 = text + l
        .map(i =>
          s"<b>${i.dayOfWeek} ${i.longDate}</b> <code>${i.from}-${i.to}</code> (${i.TicketCount})")
        .mkString("\n", "\n", "")
      markup = InlineKeyboardMarkup(l.grouped(2).map(ll => ll.map(doctorDayButton)).toSeq)
      r <- request(
        EditMessageText(chatId,
                        messageId,
                        text = text2,
                        parseMode = ParseMode.HTML,
                        replyMarkup = markup))
    } yield r
  }

  def recordDo(lpuCode: String @@ LpuCode,
               docPostId: String @@ DocPostId,
               date: String,
               posId: String @@ TimeItemId,
               time: String,
               chatId: Long,
               messageId: Long) = {

    def askPolisToRecord =
      for {
        polisList <- Repository.getPolises(chatId)
        buttons = polisList.map(pi =>
          Seq(InlineKeyboardButton(pi.alias, callbackData = CallbackCommand.RecordPolis(pi.id)))) :+
          Seq(
            InlineKeyboardButton("Ввести полис вручную",
                                 callbackData = CallbackCommand.EnterPolis(())))
        recordInfo <- Repository.retriveRecordInfo(chatId)
        text = s"<b>Записываемся к</b> <i>${recordInfo.fio}</i>\n<b>на</b> <i>${recordInfo.datetime}</i>\n<i>${recordInfo.doctorData}</i>\n<i>${recordInfo.lpuName}</i>\n\nДля записи выберите сохраненный полис или введите данные вручную"
        _ <- request(
          SendMessage(chatId, text, ParseMode.HTML, replyMarkup = InlineKeyboardMarkup(buttons)))
      } yield ()

    for {
      uc <- Repository.getUserContext(chatId)
      lpu <- Repository.getLpu(chatId, lpuCode)
      ucM = if (!uc.flow.isInstanceOf[WorkWithPolis])
        uc.modify(_.flow).setTo(WorkFlow.ShowSchedule)
      else uc
      _ <- Repository.updateUserContext(chatId,
                                        ucM
                                          .modify(_.selectedItem)
                                          .using(
                                            _.copy(cityId = lpu.map(_.CITY),
                                                   lpuCode = lpuCode,
                                                   postId = docPostId,
                                                   date = date,
                                                   posId = posId,
                                                   time = time)))
      _ <- uc.flow match {
        case WorkWithPolis(polisInfo) => makeRecord(chatId, polisInfo)
        case _ => askPolisToRecord
      }
    } yield ()
  }

  def addPolis(chatId: Long): Future[_] = {
    for {
      uc <- Repository.getUserContext(chatId)
      _ <- Repository.updateUserContext(chatId, uc.copy(flow = WorkFlow.NewPolisInfo))
      _ <- askPolisNumber(chatId)
    } yield ()
  }

  def askPolisNumber(chatId: Long): Future[_] = {
    for {
      uc <- Repository.getUserContext(chatId)
      _ <- Repository.updateUserContext(chatId, uc.copy(action = EnterPolisNumber()))
      _ <- request(SendMessage(chatId, "Введите номер полиса", ParseMode.HTML))
    } yield ()
  }

  def deletePolis(chatId: Long, messageId: Long, polis: Long): Future[_] = {
    for {
      _ <- Repository.removePolis(chatId, polis)
      polisList <- Repository.getPolises(chatId)
      _ <- request(
        EditMessageReplyMarkup(chatId,
                               messageId,
                               replyMarkup = InlineKeyboardMarkup(polisListButtons(polisList))))
    } yield ()
  }

  def polisListButtons(polisList: List[PolisInfo]): Seq[Seq[InlineKeyboardButton]] = {
    def polisButtons(polisInfo: PolisInfo): Seq[InlineKeyboardButton] =
      Seq(
        InlineKeyboardButton(polisInfo.alias,
                             callbackData = CallbackCommand.ViewPolis(polisInfo.id)),
        InlineKeyboardButton(s"${Pics.Remove} Удалить",
                             callbackData = CallbackCommand.DeletePolis(polisInfo.id))
      )

    polisList.map(polisButtons) :+ Seq(
      InlineKeyboardButton(s"${Pics.Add} Добавить новый", callbackData = AddPolis(())))
  }

  def viewPolis(chatId: Long, polisId: Long): Future[_] = {
    val polisInfoF = Repository.getPolis(chatId, polisId)
    val ucF = Repository.getUserContext(chatId)
    val r = for {
      polisInfo <- polisInfoF
      _ <- Repository.updateLocalLpus(chatId, polisInfo)
      uc <- ucF
      _ <- Repository.updateUserContext(chatId, uc.copy(flow = WorkWithPolis(polisInfo)))
      _ <- request(
        SendMessage(
          chatId,
          s"<b>Выбран полис</b> <code>${polisInfo.alias}</code>\n<b>Номер полиса:</b> <code>${polisInfo.polis}</code>\n<b>Дата рождения:</b> <code>${polisInfo.dateOfBirth}</code>",
          ParseMode.HTML,
          replyMarkup = ReplyMarkups.PolisSelected
        ))
    } yield ()
    r
  }

  private def showCityList(order: Option[Int], chatId: Long, messageId: Option[Long]) = {
    def nextButton =
      (o: Integer) => InlineKeyboardButton(s"Ещё ${Pics.Forward}", callbackData = CityOrder(o + 1))

    def showAll =
      InlineKeyboardButton(s"${Pics.Expand}", callbackData = CityAll(()))

    def prevButton =
      (o: Integer) =>
        InlineKeyboardButton(s"${Pics.Backward} Назад", callbackData = CityOrder(o - 1))

    def cityButtons =
      (city: City) =>
        Seq(InlineKeyboardButton(city.NAME, callbackData = CallbackCommand.City(city.ID)))

    def collapseButton =
      InlineKeyboardButton(s"${Pics.Collapse}", callbackData = CityOrder(1))

    def citiesMarkup(cities: Iterable[City], hasNext: Boolean): Seq[Seq[InlineKeyboardButton]] =
      order match {
        case Some(o) =>
          cities.map(cityButtons).toSeq ++
            Seq(
              ((if (o > 1) Seq(prevButton(o)) else Seq()) :+ showAll) ++
                (if (hasNext) Seq(nextButton(o)) else Seq())
            )
        case None => cities.map(cityButtons).toSeq :+ Seq(collapseButton)
      }

    val citiesF: Future[(Iterable[City], Boolean)] =
      Repository.getCities(order)

    val result: Future[ApiRequest[_]] = for {
      keyboardMarkup <- citiesF.map(c => InlineKeyboardMarkup(citiesMarkup(c._1, c._2)))
    } yield {
      messageId match {
        case None =>
          SendMessage(chatId,
                      "<b>Выберите район</b>",
                      parseMode = ParseMode.HTML,
                      replyMarkup = keyboardMarkup)
        case Some(mesId) =>
          EditMessageText(chatId,
                          mesId,
                          text = "<b>Выберите район</b>",
                          parseMode = ParseMode.HTML,
                          replyMarkup = keyboardMarkup)
      }
    }
    result.flatMap(x => request(x))
  }

  def citySelected(chatId: Long, messageId: Long, cityId: String @@ CityId) = {
    val r = for {
      uc <- Repository.getUserContext(chatId)
      _ <- Repository.updateUserContext(chatId, uc.modify(_.selectedItem.cityId).setTo(cityId))
      city <- Repository.getCity(cityId)
      mes = city match {
        case None => "<b>Ошибка при выборе города. попробуйте еще раз</b>"
        case Some(c) =>
          s"Вы выбрали <code>${c.NAME}</code>\nТеперь выберите лечебное учреждение."
      }
      _ <- request(EditMessageText(chatId, messageId, text = mes, parseMode = ParseMode.HTML))
      _ <- showLpuList(Some(1), chatId, Some(messageId), cityId)
    } yield ()
    r
  }

  def showLpuList(order: Option[Int],
                  chatId: Long,
                  messageId: Option[Long],
                  cityId: String @@ CityId) = {

    def nextButton(o: Integer) =
      InlineKeyboardButton(s"Ещё ${Pics.Forward}", callbackData = LpuOrder(o + 1, cityId))
    def showAll =
      InlineKeyboardButton(s"${Pics.Expand}", callbackData = LpuAll(cityId))
    def prevButton(o: Integer) =
      InlineKeyboardButton(s"${Pics.Backward} Назад", callbackData = LpuOrder(o - 1, cityId))
    def lpuButton(lpu: Lpu) =
      InlineKeyboardButton(lpu.NAME, callbackData = CallbackCommand.Lpu(lpu.LPUCODE))
    def collapseButton =
      InlineKeyboardButton(s"${Pics.Collapse}", callbackData = LpuOrder(1, cityId))
    def backToCitySelectButton =
      InlineKeyboardButton(s"${Pics.MenuUp} Выбрать нас. пункт", callbackData = CityOrder(1))

    def renderLpus(lpus: Iterable[Lpu], hasNext: Boolean, needBackToCity: Boolean): ApiRequest[_] = {
      val buttons = lpus.map(lpu => Seq(lpuButton(lpu))).toSeq
      val markups = buttons :+
          order.fold(Seq(collapseButton))(
            o =>
              ((if (o > 1) Seq(prevButton(o)) else Seq()) :+ showAll) ++
                (if (hasNext) Seq(nextButton(o)) else Seq())) :+
          (if (needBackToCity) Seq(backToCitySelectButton) else Seq())
      val markup = InlineKeyboardMarkup(markups)
      messageId match {
        case None =>
          SendMessage(chatId,
                      "<b>Выберите лечебное учреждение</b>",
                      parseMode = ParseMode.HTML,
                      replyMarkup = markup)
        case Some(id) =>
          EditMessageText(chatId,
                          id,
                          text = "<b>Выберите лечебное учреждение</b>",
                          parseMode = ParseMode.HTML,
                          replyMarkup = markup)
      }
    }

    val lpusF: Future[Either[String, (Iterable[Lpu], Boolean)]] =
      Repository.getLpus(chatId, cityId, order)
    val flowF = Repository.getUserContext(chatId).map(_.flow)
    val resultRequest = for {
      lpus <- lpusF
      flow <- flowF
    } yield {
      lpus match {
        case Left(error) =>
          SendMessage(chatId,
                      s"<b>Ошибка при отображении лечебных учреждений</b>\n<code>$error</code>",
                      ParseMode.HTML)
        case Right((lpuList, hasNext)) =>
          renderLpus(lpuList, hasNext, !flow.isInstanceOf[WorkWithPolis])
      }
    }
    resultRequest.flatMap(r => request(r))
  }

  def lpuSelected(chatId: Long, lpuCode: String @@ LpuCode, callbackId: String, messageId: Long) = {
    val requests: Future[Seq[ApiRequest[_]]] = for {
      lpu <- Repository.getLpu(chatId, lpuCode)
      mesReq = lpu.fold[ApiRequest[_]](
        AnswerCallbackQuery(callbackId,
                            "Ошибка при выборе лечебного учреждения. Попробуйте еще раз",
                            "true"))(
        l =>
          EditMessageText(chatId,
                          messageId,
                          text = s"Вы выбрали <code>${l.NAME}</code>",
                          parseMode = ParseMode.HTML))
      specialityReqs <- lpu
        .map(l => specialityListRequest(l, chatId, callbackId, messageId))
        .getOrElse(Future.successful(Seq()))
    } yield mesReq +: specialityReqs
    for {
      reqs <- requests
      _ <- reqs.foldLeft[Future[_]](Future.successful(()))((f, req) =>
        f.flatMap(_ => request(req)))
    } yield ()
  }

  def specialityListRequest(lpu: Lpu,
                            chatId: Long,
                            callbackId: String,
                            messageId: Long): Future[Seq[ApiRequest[_]]] = {
    def specialityButton(speciality: Speciality): Seq[InlineKeyboardButton] = {
      val volume =
        if (speciality.Volume == 0) "нет талонов" else speciality.Volume
      Seq(
        InlineKeyboardButton(speciality.Specialty.Name + s"\n(${volume})",
                             callbackData =
                               CallbackCommand.Speciality(speciality.Specialty.ID, lpu.LPUCODE)))
    }
    def backToLpuButton: Seq[InlineKeyboardButton] =
      Seq(
        InlineKeyboardButton(s"${Pics.MenuUp} К выбору учреждения",
                             callbackData = CallbackCommand.LpuOrder(1, lpu.CITY)))

    val specialities = Repository.getSpecialities(lpu.LPUCODE)
    val result: Future[Seq[ApiRequest[_]]] = for {
      specList <- specialities
      markups = specList.sortBy(_.Specialty.Name).map(specialityButton) :+ backToLpuButton
    } yield {
      val editMarkup: ApiRequest[_] =
        EditMessageReplyMarkup(chatId, messageId, replyMarkup = InlineKeyboardMarkup(markups))
      val alert =
        if (specList.nonEmpty) Seq()
        else
          Seq(
            AnswerCallbackQuery(callbackId,
                                "В выбранном лечебном учреждении нет доступных специальностей",
                                "true"))
      alert :+ editMarkup
    }
    result
  }

  private def doctorText(doctor: DocPost) = {
    s"${Pics.Person}<b>${doctor.Doctor.Family} ${doctor.Doctor.Name} ${doctor.Doctor.Patronymic}</b>\n" +
      s"${doctor.Post}\n" +
      s"${doctor.Sepatation} каб. ${doctor.Room}\n" +
      s"Доступно (<code>${doctor.TicketCount}</code>)"
  }
  def specialitySelected(specId: String @@ SpecialityId,
                         lpuCode: String @@ LpuCode,
                         chatId: Long,
                         callbackId: String,
                         messageId: Long) = {

    def getScheduleButton(doctor: DocPost) =
      InlineKeyboardButton("Получить расписание", callbackData = Doctors(doctor.ID, lpuCode))

    def markup(doctor: DocPost) =
      InlineKeyboardMarkup(Seq(Seq(getScheduleButton(doctor))))

    val r = Repository.getSpecialityInfo(chatId, specId, lpuCode).flatMap {
      case (speciality, lpu, docList) =>
        if (docList.isEmpty)
          request(
            AnswerCallbackQuery(callbackId, "По данной специальности никого не найдено", "true"))
        else {
          val updatedContext =
            (uc: UserContext) => uc.modify(_.selectedItem.specId).setTo(specId)
          val updateUserContext = Repository
            .getUserContext(chatId)
            .flatMap(uc => Repository.updateUserContext(chatId, updatedContext(uc)))
          val m = request(
            EditMessageText(
              chatId,
              text = s"${Pics.Speciality} Выбран <b>${speciality.Specialty.Name}</b>",
              messageId = messageId,
              parseMode = ParseMode.HTML))
          val doctorMessage = (doctor: DocPost) =>
            request(
              SendMessage(chatId,
                          text = doctorText(doctor),
                          parseMode = ParseMode.HTML,
                          replyMarkup = markup(doctor)))
          for {
            _ <- updateUserContext
            _ <- m
          } yield Future.sequence(docList.map(doctorMessage))
        }
    }
    r
  }

  def showSchedule(lpuCode: String @@ LpuCode,
                   doctorPostId: String @@ DocPostId,
                   date: String,
                   chatId: Long,
                   messageId: Long) = {
    def scheduleButton(d: TimeItem) =
      InlineKeyboardButton(
        text = d.time,
        callbackData = CallbackCommand.Record(lpuCode, doctorPostId, date, d.attrs.PosID, d.time))

    val backButton = InlineKeyboardButton(text = s"${Pics.Backward} Назад",
                                          callbackData = Doctors(doctorPostId, lpuCode))
    for {
      l <- Repository.getSchedule(chatId, lpuCode, doctorPostId, date)
      selectedItem <- Repository.getUserContext(chatId).map(_.selectedItem)
      doctorPost <- Repository
        .doctors(selectedItem.specId.get, lpuCode)
        .map(_.find(_.ID == doctorPostId).get)
      text = doctorText(doctorPost) + s"\nВыбранный день <code>$date</code>"
      replyMarkup = InlineKeyboardMarkup(l.map(scheduleButton).grouped(4).toSeq :+ Seq(backButton))
      r <- request(
        EditMessageText(chatId,
                        messageId,
                        text = text,
                        parseMode = ParseMode.HTML,
                        replyMarkup = replyMarkup))
    } yield r
  }

  def scheduleSelected(lpuCode: String @@ LpuCode,
                       doctorPostId: String @@ DocPostId,
                       date: String,
                       posId: String @@ TimeItemId,
                       time: String,
                       chatId: Long,
                       messageId: Long): Future[_] = {
    val confirmButton = InlineKeyboardButton(
      text = "Записаться",
      callbackData = CallbackCommand.RecordDo(lpuCode, doctorPostId, date, posId, time))
    val backButton = InlineKeyboardButton(text = s"${Pics.Backward} Назад",
                                          callbackData =
                                            CallbackCommand.Schedule(lpuCode, doctorPostId, date))
    for {
      selectedItem <- Repository.getUserContext(chatId).map(_.selectedItem)
      doctorPost <- Repository
        .doctors(selectedItem.specId.get, lpuCode)
        .map(_.find(_.ID == doctorPostId).get)
      text = doctorText(doctorPost) + s"\nВыбранное время <code>$date $time</code>"
      markup = InlineKeyboardMarkup(Seq(Seq(backButton, confirmButton)))
      r <- request(
        EditMessageText(chatId,
                        messageId,
                        text = text,
                        parseMode = ParseMode.HTML,
                        replyMarkup = markup))
    } yield r
  }

  override def handleFuture(f: Future[_], chatId: Long): Unit = {
    //val ff = f andThen { case Failure(_) => repository.updateUserContext(chatId, UserContext()).failed.foreach(_.printStackTrace) }
    super.handleFuture(f, chatId)
  }

  override def handleMessageInt(message: Message): Option[Future[_]] = {
    val f = super.handleMessageInt(message)

    def handlePolisActions(context: UserContext): Future[_] =
      context.action match {
        case EnterPolisNumber(pi) => polisNumberEntered(message, context)
        case EnterDateOfBirth(pi) => birthDateEntered(message, pi, context)
        case EnterAlias(pi) => aliasEntered(message, pi, context)
        case _ => Future(())
      }

    def customAction: Future[_] =
      Repository.getUserContext(message.sender) flatMap (ctx => {
                                                           ctx.flow match {
                                                             case WorkFlow.NewPolisInfo |
                                                                 WorkFlow.ShowSchedule =>
                                                               handlePolisActions(ctx)
                                                             case _ =>
                                                               Future(())
                                                           }
                                                         })

    f.orElse(customAction)
  }

  def polisNumberEntered(message: Message, userContext: UserContext): Future[Unit] = {
    val action = EnterDateOfBirth(PolisInfo(polis = message.text.getOrElse("")))
    for {
      _ <- Repository.updateUserContext(message.sender, userContext.copy(action = action))
      _ <- request(
        SendMessage(
          message.sender,
          s"Номер полиса: <b>${action.polisInfo.polis}</b>\nВведите дату рождения в формате <b>01.10.2001</b>",
          ParseMode.HTML))
    } yield ()
  }

  def birthDateEntered(message: Message,
                       polisInfo: PolisInfo,
                       userContext: UserContext): Future[_] = {
    val birthDateStr = message.text.getOrElse("")
    val onSuccess: (String) => Future[_] = (birthDate: String) => {
      val enterAliasText =
        "\n<b>Введите имя</b> (может быть любым для вашего удобства)"
      val selectedText = s"Дата рождения: <b>${birthDate}</b>"
      val newPolisInfo = polisInfo.copy(dateOfBirth = birthDate)
      val (action, text) = userContext.flow match {
        case ShowSchedule => (EnterDateOfBirth(newPolisInfo), selectedText)
        case NewPolisInfo =>
          (EnterAlias(newPolisInfo), selectedText + enterAliasText)
        case _ => (Empty, "")
      }
      for {
        _ <- Repository.updateUserContext(message.sender, userContext.copy(action = action))
        _ <- request(SendMessage(message.sender, text, ParseMode.HTML))
        _ <- userContext.flow match {
          case ShowSchedule => makeRecord(message.sender, newPolisInfo)
          case _ => Future {}
        }
      } yield ()
    }
    val onError: (Throwable) => Future[_] = (error: Throwable) => {
      error.printStackTrace()
      request(
        SendMessage(
          message.sender,
          s"Введена неверная дата рождения: <b>${birthDateStr}</b>\nВведите дату рождения в формате <b>01.10.2001</b>",
          ParseMode.HTML))
    }
    val birthDate = Try(LocalDate.parse(birthDateStr, DateTimeFormatter.ofPattern("dd.MM.yyyy")))
    birthDate match {
      case Success(_) => onSuccess(birthDateStr)
      case Failure(ex) => onError(ex)
    }
  }

  def makeRecord(chatId: Long, polisInfo: PolisInfo): Future[Unit] = {
    val errorMessage = (error: String) =>
      s"<b>Произошла ошибка при выполнении записи:</b>\n<code>$error</code>"
    val resultMessage = (recordResult: RecordResult) =>
      s"Запись на прием успешно произведена\n" + recordResult.toText
    for {
      _ <- Repository.prepareSubmit(chatId)
      res <- Repository.submit(chatId, polisInfo)
      message = res.fold(errorMessage, resultMessage)
      _ <- request(SendMessage(chatId, message, ParseMode.HTML))
    } yield ()
  }

  def aliasEntered(message: Message, polisInfo: PolisInfo, userContext: UserContext) = {
    val newPolisInfo = polisInfo.copy(alias = message.text.getOrElse(""))
    val action = EnterAlias(newPolisInfo)
    for {
      _ <- Repository.updateUserContext(message.sender, userContext.copy(action = action))
      _ <- Repository.savePolis(message.sender, newPolisInfo)
      _ <- request(
        SendMessage(
          message.sender,
          s"Полис с именем <b>${action.polisInfo.alias}</b> сохранен.\n<b>Номер полиса:</b>${action.polisInfo.polis}\n<b>Дата рождения:</b>${action.polisInfo.dateOfBirth}",
          ParseMode.HTML
        ))
    } yield ()
  }

  val cancelButton =
    (lpuCode: String @@ LpuCode, recordId: String @@ RecordId) =>
      Seq(
        Seq(
          InlineKeyboardButton("Отменить приём",
                               callbackData = CallbackCommand.CancelRecord(recordId, lpuCode))))

  def showRecords(chatId: Long): Future[_] = {
    def message(recordResult: RecordResult): SendMessage = {
      val buttons = cancelButton(recordResult.lpuCode, recordResult.id)
      SendMessage(chatId,
                  recordResult.toText,
                  ParseMode.HTML,
                  replyMarkup = InlineKeyboardMarkup(buttons))
    }

    val emptyMessage: SendMessage =
      SendMessage(chatId, "<b>Записей нет</b>", ParseMode.HTML)
    for {
      records <- Repository.retrieveRecords(chatId)
      messages = records.map(message)
      _ <- if (messages.isEmpty) request(emptyMessage)
      else
        messages.foldLeft[Future[_]](Future.successful(()))((f, m) => f.flatMap(_ => request(m)))
    } yield ()
  }

  def cancelShowConfirm(messageId: Long,
                        chatId: Long,
                        lpuCode: String @@ LpuCode,
                        recordId: String @@ RecordId): Future[_] = {
    val buttons = Seq(
      Seq(
        InlineKeyboardButton(s"${Pics.Delete} Подтвердить удаление",
                             callbackData = CallbackCommand.CancelConfirm(recordId, lpuCode))),
      Seq(
        InlineKeyboardButton(s"${Pics.Backward} Назад",
                             callbackData = CallbackCommand.CancelBack(recordId, lpuCode)))
    )
    request(EditMessageReplyMarkup(chatId, messageId, replyMarkup = InlineKeyboardMarkup(buttons)))
  }

  def cancelBack(messageId: Long,
                 chatId: Long,
                 lpuCode: String @@ LpuCode,
                 recordId: String @@ RecordId): Future[_] = {
    val buttons = cancelButton(lpuCode, recordId)
    request(EditMessageReplyMarkup(chatId, messageId, replyMarkup = InlineKeyboardMarkup(buttons)))
  }

  def cancelRecord(messageId: Long,
                   chatId: Long,
                   lpuCode: String @@ LpuCode,
                   recordId: String @@ RecordId): Future[_] = {
    val messageRecordCanceled = EditMessageText(chatId,
                                                messageId,
                                                text = "<b>Запись отменена</b>",
                                                parseMode = ParseMode.HTML)
    val messageRecordCancelFailed = (error: String) =>
      EditMessageText(chatId,
                      messageId,
                      text = s"<b>Ошибка:</b>\n<code>$error</code>",
                      parseMode = ParseMode.HTML)
    for {
      cancelResult <- Repository.cancelRecord(chatId, lpuCode, recordId)
      message = cancelResult.fold(messageRecordCanceled)(messageRecordCancelFailed)
      _ <- request(message)
    } yield ()
  }
}

object Main extends App {
  EntryToDocBot.run()
}
