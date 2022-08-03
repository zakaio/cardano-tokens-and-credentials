package proofspace.cardano.tokenConnector

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.*
import akka.http.scaladsl.server.Directives.*
import akka.http.scaladsl.server.Route

import scala.concurrent.*
import scala.concurrent.duration.*

import cps.*
import cps.monads.{*, given}

import proofspace.cardano.tokenConnector.dto.*
import proofspace.cardano.tokenConnector.util.*


object Main {

    implicit val actorSystem: ActorSystem = ActorSystem()
    import actorSystem.dispatcher

    val route =
        path("hello") {
          get {
            complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, "<h1>Say hello to akka-http</h1>"))
          }
        }

    val shutdownPromise = Promise[String]()    

    def main(args: Array[String]): Unit = {    
        
        val bindingFuture = Http().newServerAt("localhost", 8080).bind(route)
        println(s"Server now online. Please navigate to http://localhost:8080/hello")

        val msg = Await.result(shutdownPromise.future, Duration.Inf)
        println(s"finishing ($msg)")

        bindingFuture.flatMap(_.unbind()).onComplete(_ => actorSystem.terminate()) 

    }

}

