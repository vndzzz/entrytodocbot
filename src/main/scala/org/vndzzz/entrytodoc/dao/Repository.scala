package org.vndzzz.entrytodoc.dao

import akka.http.scaladsl.model.headers._
import com.google.common.cache.{Cache, CacheBuilder}
import com.softwaremill.tagging.{@@, _}
import org.vndzzz.entrytodoc.GlobalImplicits
import org.vndzzz.entrytodoc.entity._
import org.vndzzz.entrytodoc.service.MosregRequest._
import org.vndzzz.entrytodoc.service.MosregService._
import org.vndzzz.entrytodoc.user._

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.Try
import scalacache._
import scalacache.guava._
import scalacache.memoization._

/**
  * Created by vn on 19.11.2016.
  */
object Repository extends GlobalImplicits {

  import org.vndzzz.entrytodoc.bot.EntryToDocBot._
  //need to import akka defaults

  val underlyingGuavaCache: Cache[String, Object] =
    CacheBuilder.newBuilder().maximumSize(10000).build[String, Object]
  implicit val scalaCache = ScalaCache(GuavaCache(underlyingGuavaCache))

  private val doctorsCacheName: String = "doctors"
  private val doctorDatesCacheName: String = "doctorDates"
  private val userContextCacheName: (Long) => String = (chatId: Long) => s"userContext$chatId"
  private val polisListCacheName: (Long) => String = (chatId: Long) => s"polisList$chatId"
  private val polisCache: (Long) => String = (polisId: Long) => s"polis$polisId"
  private val cookieCache: (Long) => String = (chatId: Long) => s"cookie$chatId"
  private val localLpusCache: (Long) => String = (chatId: Long) => s"localLpu$chatId"

  private val indexToOrder = (index: Int) => math.ceil((index + 1).toDouble / 7).toInt

  def getCities(order: Option[Int]): Future[(List[City], Boolean)] = {
    getCitiesAll.map { cities =>
      order.fold((cities, false)) { o =>
        val values = cities.filter(_.order.exists(_ == o))
        val hasNext = cities.exists(_.order.exists(_ > o))
        (values, hasNext)
      }
    }
  }

  private def getCitiesAll: Future[List[City]] = memoize(12 hours) {
    CityRequest.run.map(_.zipWithIndex.map {
      case (city, index) => city.copy(order = Some(indexToOrder(index)))
    })
  }

  def getLpus(chatId: Long,
              cityId: String @@ CityId,
              order: Option[Int]): Future[Either[String, (Iterable[Lpu], Boolean)]] = {
    for {
      uc <- getUserContext(chatId)
      allLpus: Either[String, Iterable[Lpu]] <- uc.flow match {
        case WorkFlow.WorkWithPolis(polisInfo) =>
          getLocalLpus(chatId, polisInfo)
        case _ => getAllLpu(cityId).map(ml => Right(ml.values))
      }
    } yield
      allLpus.right.map { lpus =>
        order.fold((lpus, false)) { o =>
          val values = lpus.filter(_.order.exists(_ == o))
          val hasNext = lpus.exists(_.order.exists(_ > o))
          (values, hasNext)
        }
      }
  }

  private def getAllLpu(cityId: String @@ CityId): Future[Map[String, Lpu]] =
    memoize(24 hours) {
      val lpuListF = LpuListRequest(cityId).run.map(_.map(_._2))
      lpuListF.map { lpuList =>
        lpuList.zipWithIndex.map {
          case (lpu, index) =>
            (lpu.LPUCODE, lpu.copy(order = Some(indexToOrder(index))))
        }.toMap
      }
    }

  def getSpecialities(lpuCode: String @@ LpuCode): Future[List[Speciality]] =
    SpecialityRequest(lpuCode).run

  def getDoctorDates(chatId: Long, docPostId: String @@ DocPostId): Future[List[DoctorDayInfo]] = {
    for {
      selectedItem <- getUserContext(chatId).map(_.selectedItem)
      result <- doctorDates(selectedItem.specId.getOrElse("".taggedWith[SpecialityId]),
                            selectedItem.lpuCode.getOrElse("".taggedWith[LpuCode]))
        .map(_(docPostId))
    } yield result
  }

  def getSpecialityInfo(chatId: Long,
                        specId: String @@ SpecialityId,
                        lpuCode: String @@ LpuCode): Future[(Speciality, Lpu, List[DocPost])] = {
    for {
      r <- DocPostRequest(lpuCode, specId).run
      _ <- put(doctorsCacheName)(r.docPostList, Some(10 minutes))
      _ <- put(doctorDatesCacheName)(r.docDates, Some(2 minutes))
    } yield (r.speciality, r.lpu, r.docPostList)
  }

  def doctors(specId: String @@ SpecialityId, lpuCode: String @@ LpuCode) = {
    get[List[DocPost], NoSerialization](doctorsCacheName)
      .flatMap(_.fold(DocPostRequest(lpuCode, specId).run.map(_.docPostList))(Future.successful))
  }

  private def doctorDates(specId: String @@ SpecialityId, lpuCode: String @@ LpuCode) = {
    get[Map[String, List[DoctorDayInfo]], NoSerialization](doctorDatesCacheName)
      .flatMap(_.fold(DocPostRequest(lpuCode, specId).run.map(_.docDates))(Future.successful))
  }

  private def saveCookie(chatId: Long, httpCookie: HttpCookie): Future[Unit] = {
    CookieDB
      .save(chatId, httpCookie)
      .flatMap(_ => put(cookieCache(chatId))(Cookie(httpCookie.name, httpCookie.value)))
  }

  def getSchedule(chatId: Long,
                  lpuCode: String @@ LpuCode,
                  doctorPostId: String @@ DocPostId,
                  date: String): Future[List[TimeItem]] = {
    for {
      (result, cookie) <- ScheduleRequest(lpuCode, doctorPostId, date).runWithCookie
      _ <- saveCookie(chatId, cookie())
    } yield {
      result
    }
  }

  def getCity(cityId: String @@ CityId): Future[Option[City]] =
    getCitiesAll.map(_.find(_.ID == cityId))

  def updateUserContext(chatId: Long, userContext: UserContext): Future[_] = {
    UserContextDB
      .updateContext(chatId, userContext)
      .flatMap(_ => put(userContextCacheName(chatId))(userContext, Some(4 hours)))
  }

  def prepareSubmit(chatId: Long): Future[Unit] = {
    val selectedItemF = getUserContext(chatId).map(_.selectedItem)
    val cookieF = getCookie(chatId)
    for {
      item <- selectedItemF
      cookie <- cookieF
      u <- LastStepRequest(item.cityId.get,
                           item.lpuCode.get,
                           item.specId.get,
                           item.postId.get,
                           item.date.get,
                           item.posId.get,
                           cookie).run
    } yield u
  }

  def submit(chatId: Long, polisInfo: PolisInfo): Future[Either[String, RecordResult]] = {
    val selectedItemF = getUserContext(chatId).map(_.selectedItem)
    val cookieF = getCookie(chatId)
    val r: Future[Either[String, Future[RecordResult]]] = for {
      item <- selectedItemF
      cookie <- cookieF
      _ <- SubmitRequest(polisInfo,
                         item.cityId.get,
                         item.lpuCode.get,
                         item.specId.get,
                         item.postId.get,
                         item.date.get,
                         item.posId.get,
                         cookie).run
      result <- CreateVisitRequest(polisInfo,
                                   item.cityId.get,
                                   item.lpuCode.get,
                                   item.specId.get,
                                   item.postId.get,
                                   item.date.get,
                                   item.posId.get,
                                   cookie).run
    } yield {
      result.right.map { stubNum =>
        for {
          lpu <- getLpu(chatId, item.lpuCode.get)
          doctorPost <- doctors(item.specId.getOrElse("".taggedWith[SpecialityId]),
                                item.lpuCode.getOrElse("".taggedWith[LpuCode]))
            .map(_.find(_.ID == item.postId.get))
        } yield
          RecordResult(
            datetime = s"${item.date.get} ${item.time.get}",
            fio =
              s"${doctorPost.get.Doctor.Family}  ${doctorPost.get.Doctor.Name} ${doctorPost.get.Doctor.Patronymic}",
            doctorData =
              s"${doctorPost.get.Post}\n${doctorPost.get.Sepatation} каб.${doctorPost.get.Room}",
            lpuName = lpu.get.NAME,
            lpuAddress = lpu.get.ADDRESS,
            pol = polisInfo.polis,
            stubNum = stubNum,
            polisAlias = polisInfo.alias
          )
      }
    }
    r.flatMap {
      case Left(error) => Future(Left(error))
      case Right(f) => f.map(rr => Right(rr))
    }
  }

  def retriveRecordInfo(chatId: Long): Future[RecordInfo] = {
    for {
      item <- getUserContext(chatId).map(_.selectedItem)
      lpu <- getLpu(chatId, item.lpuCode.get)
      doctorPost <- doctors(item.specId.getOrElse("".taggedWith[SpecialityId]),
                            item.lpuCode.getOrElse("".taggedWith[LpuCode]))
        .map(_.find(_.ID == item.postId.get))
    } yield
      RecordInfo(
        datetime = s"${item.date.get} ${item.time.get}",
        fio =
          s"${doctorPost.get.Doctor.Family}  ${doctorPost.get.Doctor.Name} ${doctorPost.get.Doctor.Patronymic}",
        doctorData =
          s"${doctorPost.get.Post}\n${doctorPost.get.Sepatation} каб.${doctorPost.get.Room}",
        lpuName = lpu.get.NAME
      )
  }

  def getUserContext(chatId: Long): Future[UserContext] =
    cachingWithTTL(userContextCacheName(chatId))(4 hours) {
      UserContextDB.userContext(chatId).map(_.getOrElse(UserContext()))
    }

  def getLpu(chatId: Long, lpuCode: String @@ LpuCode): Future[Option[Lpu]] = {
    for {
      uc <- getUserContext(chatId)
      item = uc.selectedItem
      lpu <- uc.flow match {
        case WorkFlow.WorkWithPolis(polisInfo) =>
          getLocalLpus(chatId, polisInfo).map {
            case Left(_) => None
            case Right(lpus) => lpus.find(_.LPUCODE == lpuCode)
          }
        case _ =>
          getAllLpu(item.cityId.getOrElse("".taggedWith[CityId]))
            .map(_.get(lpuCode))
      }
    } yield lpu
  }

  def savePolis(chatId: Long, polisInfo: PolisInfo): Future[_] = {
    PolisInfoDB.save(chatId, polisInfo).flatMap { _ =>
      val r = remove(polisListCacheName(chatId))
      val m = put(polisCache(polisInfo.id))(polisInfo)
      r.flatMap(_ => m)
    }
  }

  def getPolises(chatId: Long): Future[List[PolisInfo]] =
    caching(polisListCacheName(chatId)) {
      PolisInfoDB.all(chatId).map(_.sortBy(_.alias))
    }

  def removePolis(chatId: Long, polisId: Long): Future[_] =
    PolisInfoDB.delete(chatId, polisId).flatMap { _ =>
      val r = remove(polisListCacheName(chatId))
      val r2 = remove(polisCache(polisId))
      r.flatMap(_ => r2)
    }

  def getPolis(chatId: Long, polisId: Long): Future[PolisInfo] =
    caching(polisCache(polisId)) {
      PolisInfoDB.find(polisId)
    }

  def getLocalLpus(chatId: Long, polisInfo: PolisInfo): Future[Either[String, List[Lpu]]] =
    cachingWithTTL(localLpusCache(chatId))(2 hours) {
      updateLocalLpus(chatId, polisInfo)
    }

  def updateLocalLpus(chatId: Long, polisInfo: PolisInfo): Future[Either[String, List[Lpu]]] = {
    val f = LocalLpuRequest(polisInfo).runWithCookie
    val res = for {
      (lpusE, cookie) <- f
    } yield {
      val saveCookieF = saveCookie(chatId, cookie())
      val invalidateCache = remove(localLpusCache(chatId))
      val lpuList = lpusE.right.map(lpus =>
        lpus.zipWithIndex.map {
          case (lpu, index) => lpu.copy(order = Some(indexToOrder(index)))
      })
      saveCookieF.flatMap(_ => invalidateCache).map(_ => lpuList)
    }
    res.flatMap(x => x)
  }

  def retrieveRecords(chatId: Long): Future[List[RecordResult]] = {
    val cookieF = getCookie(chatId)
    val contextF = getUserContext(chatId)
    for {
      cookie <- cookieF
      alias <- contextF.map(_.flow).collect {
        case WorkFlow.WorkWithPolis(polis) => Some(polis.alias)
        case _ => None
      }
      result <- Records(cookie).run
    } yield alias.fold(result)(r => result.map(_.copy(polisAlias = r)))
  }

  def getCookie(chatId: Long): Future[Cookie] =
    caching(cookieCache(chatId))(CookieDB.get(chatId))

  def cancelRecord(chatId: Long,
                   lpuCode: String @@ LpuCode,
                   recordId: String @@ RecordId): Future[Option[String]] =
    for {
      cookie <- getCookie(chatId)
      result <- Cancel(cookie, lpuCode, recordId).run
    } yield result

  def putMetrics(chatId: Long, json: String, name: String) = Try{
    /*val token = ""
    val uri = Uri(URLEncoder.encode(s"https://api.botan.io/track?token=$token&uid=$chatId&name=$name", "UTF-8"))
    val entity = HttpEntity(ContentTypes.`application/json`, json)
    for {
      response <- Http().singleRequest(HttpRequest(HttpMethods.POST, uri, entity = entity))
    } response.discardEntityBytes()
    */
  }
}
