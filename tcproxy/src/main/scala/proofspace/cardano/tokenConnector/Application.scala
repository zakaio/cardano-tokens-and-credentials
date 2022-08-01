package proofspace.cardano.tokenConnector

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Route

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.*
import sttp.client3.*

import cps.*
import cps.monads.{*, given}

import proofspace.cardano.tokenConnector.dto.*

object Main {

    implicit val actorSystem: ActorSystem = ActorSystem()
    import actorSystem.dispatcher

    val submitDidEndpoint: PublicEndpoint[String, HttpErrorDTO, String, Any] =
         endpoint.post.in("submitDid").in(query[String]("did")).out(stringBody).errorOut(jsonBody[HttpErrorDTO])

    def submitDidAction(did: String): Future[Either[HttpErrorDTO,String]] = async[Future] {
         Right(s"${did} submitted")
    }     

    val submitRoute: Route = 
            AkkaHttpServerInterpreter().toRoute(submitDidEndpoint.serverLogic(submitDidAction))
          


}

