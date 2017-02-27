package org.vndzzz.entrytodoc.dao

import java.util.concurrent.Executors

import akka.http.scaladsl.model.headers.{Cookie, HttpCookie}
import com.typesafe.config.ConfigFactory
import io.getquill.{PostgresAsyncContext, SnakeCase}
import org.vndzzz.entrytodoc.user.{PolisInfo, UserContext}

import scala.concurrent.{ExecutionContext, Future}

/**
  * Created by vn on 12.12.2016.
  */
trait DB {
  lazy val appConfig = ConfigFactory.defaultApplication()
  lazy val secretConfig =
    if (System.getProperty("mode") == "prod") ConfigFactory.load("prod")
    else ConfigFactory.load("dev")
  lazy val ctx =
    new PostgresAsyncContext[SnakeCase](appConfig.withFallback(secretConfig).getConfig("db"))
  implicit val ec = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())
}

object UserContextDB extends DB {

  import ctx._

  case class UserContextContainer(chatId: Long, context: String)

  private val tableContext = quote {
    querySchema[UserContextContainer]("usercontext")
  }

  def userContext(chatId: Long): Future[Option[UserContext]] = {
    val q = quote {
      tableContext.filter(_.chatId == lift(chatId))
    }
    ctx.run(q).map(list => list.headOption.map(ucc => UserContext.fromJsonString(ucc.context)))
  }

  def updateContext(chatId: Long, userContext: UserContext): Future[_] = {
    val updateQ = quote {
      tableContext
        .filter(_.chatId == lift(chatId))
        .update(_.context -> lift(userContext.toJsonString))
    }
    ctx.run(updateQ)
  }

  def insertContext(chatId: Long, userContext: UserContext): Future[_] = {
    val insertQ = quote {
      tableContext.insert(lift(UserContextContainer(chatId, userContext.toJsonString)))
    }
    ctx.run(insertQ).fallbackTo(updateContext(chatId, userContext))
  }
}

object PolisInfoDB extends DB {

  import ctx._

  case class PolisInfoContainer(chatId: Long, polisInfo: PolisInfo)

  private val tableContext = quote {
    querySchema[PolisInfoContainer]("polisinfo")
  }

  def all(chatId: Long): Future[List[PolisInfo]] = {
    val searchQ = quote {
      tableContext.filter(_.chatId == lift(chatId)).map(_.polisInfo)
    }
    ctx.run(searchQ)
  }

  def delete(chatId: Long, polisId: Long) = {
    val deleteQ = quote {
      tableContext
        .filter(x => x.chatId == lift(chatId) && (x.polisInfo.id == lift(polisId)))
        .delete
    }
    ctx.run(deleteQ)
  }

  def save(chatId: Long, polisInfo: PolisInfo) = {
    val saveQ = quote {
      tableContext
        .insert(
          _.chatId -> lift(chatId),
          _.polisInfo.polis -> lift(polisInfo.polis),
          _.polisInfo.alias -> lift(polisInfo.alias),
          _.polisInfo.dateOfBirth -> lift(polisInfo.dateOfBirth)
        )
        .returning(_.polisInfo.id)
    }
    ctx.run(saveQ)
  }

  def find(polisId: Long): Future[PolisInfo] = {
    val findQ = quote {
      tableContext.filter(_.polisInfo.id == lift(polisId)).map(_.polisInfo)
    }
    ctx.run(findQ).map(_.head)
  }
}

object CookieDB extends DB {

  import ctx._

  case class CookieContainer(chatId: Long, name: String, value: String)

  private val schema = quote {
    querySchema[CookieContainer]("cookie")
  }

  def save(chatId: Long, httpCookie: HttpCookie): Future[_] = {
    val insertQ = quote {
      schema.insert(lift(CookieContainer(chatId, httpCookie.name, httpCookie.value)))
    }
    val updateQ = quote {
      schema
        .filter(_.chatId == lift(chatId))
        .update(_.name -> lift(httpCookie.name), _.value -> lift(httpCookie.value))
    }
    ctx.run(insertQ).fallbackTo(ctx.run(updateQ))
  }

  def get(chatId: Long): Future[Cookie] = {
    val getQ = quote {
      schema.filter(_.chatId == lift(chatId))
    }
    ctx.run(getQ).map { l =>
      val cc = l.head
      Cookie(cc.name, cc.value)
    }
  }
}
