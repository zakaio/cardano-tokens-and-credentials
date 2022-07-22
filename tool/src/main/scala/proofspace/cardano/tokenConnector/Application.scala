package proofspace.cardano.tokenConnector

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Route
import sttp.tapir._
import sttp.tapir.server.akkahttp.AkkaHttpServerInterpreter

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import sttp.client3._

object Main {

    implicit val actorSystem: ActorSystem = ActorSystem()
    import actorSystem.dispatcher

    val publish: PublicEndpoint[String, Unit, String, Any] =
         endpoint.post.in("publish").in(query[String]("name")).out(stringBody)



}

