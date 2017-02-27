package org.vndzzz.entrytodoc.service

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.headers._
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.Materializer

import scala.concurrent.Future

/**
  * Created by vn on 20.12.2016.
  */
object MosregService {

  implicit class MosregRequestOps[R <: MosregRequest[_]](request: R)(implicit actorSystem: ActorSystem, materializer: Materializer) {

    import actorSystem.dispatcher

    def runWithCookie[Response <: request.Response](implicit reader: JsonReader[Response]): Future[(Response, () => HttpCookie)] = {
      for {
        response <- Http().singleRequest(request.httpRequest)
        _ = if (!response.status.isSuccess()) response.discardEntityBytes()
        if response.status.isSuccess()
        result <- Unmarshal(response).to[String]
        cookie = response.headers.collect { case a: `Set-Cookie` => a.cookie }
      } yield (reader.read(result), () => cookie.head)
    }

    def run[Response <: request.Response](implicit reader: JsonReader[Response]): Future[Response] = runWithCookie.map(_._1)
  }
}
