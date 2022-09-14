package proofspace.cardano.tokenConnector


import scala.concurrent.*
import scala.concurrent.duration.*

import cps.*
import cps.monads.{*, given}

import sttp.tapir.*
import sttp.tapir.server.netty.{NettyFutureServer, NettyFutureServerOptions}

import proofspace.cardano.tokenConnector.dto.*
//import proofspace.cardano.tokenConnector.util.*


object Main {

  
    val shutdownPromise = Promise[String]()   
    
    val helloWorldEndpoint: PublicEndpoint[String, Unit, String, Any] =
        endpoint.get.in("hello").in(query[String]("name")).out(stringBody)

    val helloWorldServerEndpoint = helloWorldEndpoint
        .serverLogic(name => Future.successful[Either[Unit, String]](Right(s"Hello, $name!")))
    


    def main(args: Array[String]): Unit = {    

        val port = 9090
        val host = "localhost"

        // no time to think
        given ExecutionContext = scala.concurrent.ExecutionContext.global

        
        val serverFuture = NettyFutureServer()
                                 .port(port)
                                 .host(host)
                                 .addEndpoint(helloWorldServerEndpoint)
                                 .start()
        val bindings = Await.result(serverFuture, 60.seconds) 
        println(s"Server now online. Please navigate to http://${bindings.hostName}:${bindings.port}/hello")

        val msg = Await.result(shutdownPromise.future, Duration.Inf)
        println(s"finishing ($msg)")

        val stopFuture = bindings.stop()
        Await.ready(stopFuture, 600.seconds)

    }

}

