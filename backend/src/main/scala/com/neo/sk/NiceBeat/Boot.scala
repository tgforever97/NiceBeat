package com.neo.sk.NiceBeat

import java.util.concurrent.TimeUnit

import akka.actor.ActorSystem
import akka.dispatch.MessageDispatcher
import akka.event.{Logging, LoggingAdapter}
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import akka.util.Timeout
import com.neo.sk.NiceBeat.http.HttpService

import scala.language.postfixOps
import scala.util.{Failure, Success}
import akka.actor.ActorSystem
import akka.actor.typed.ActorRef
import akka.stream.scaladsl._
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import akka.actor.typed.scaladsl.adapter._
import com.neo.sk.NiceBeat.core._


import scala.concurrent.duration._



/**
  * Created by hongruying on 2018/3/11
  */
object Boot extends HttpService {

  import com.neo.sk.NiceBeat.common.AppSettings._
  import concurrent.duration._

  override implicit val system = ActorSystem("tankDemoSystem", config)
  // the executor should not be the default dispatcher.
  override implicit val executor: MessageDispatcher =
    system.dispatchers.lookup("akka.actor.my-blocking-dispatcher")

  override implicit val materializer = ActorMaterializer()

  override implicit val scheduler = system.scheduler

  override implicit val timeout:Timeout = Timeout(20 seconds) // for actor asks

  val log: LoggingAdapter = Logging(system, getClass)

  val roomManager:ActorRef[RoomManager.Command] = system.spawn(RoomManager.create(),"roomManager")

  val userManager:ActorRef[UserManager.Command] = system.spawn(UserManager.create(),"userManager")

  def main(args: Array[String]) {
    log.info("Starting.")
    val binding = Http().bindAndHandle(routes, httpInterface, httpPort)
    binding.onComplete {
      case Success(b) ⇒
        val localAddress = b.localAddress
        println(s"Server is listening on ${localAddress.getHostName}:${localAddress.getPort}")
      case Failure(e) ⇒
        println(s"Binding failed with ${e.getMessage}")
        system.terminate()
        System.exit(-1)
    }
  }


}
