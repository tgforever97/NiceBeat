package com.neo.sk.NiceBeat.http

import java.net.URLEncoder

import akka.actor.{ActorSystem, Scheduler}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.ws.Message
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.stream.Materializer
import akka.stream.scaladsl.Flow
import akka.util.Timeout
import com.neo.sk.NiceBeat.common.AppSettings
import akka.actor.typed.scaladsl.AskPattern._
import com.neo.sk.NiceBeat.Boot.roomManager

import scala.concurrent.{ExecutionContextExecutor, Future}
import com.neo.sk.NiceBeat.Boot.{executor, scheduler, timeout, userManager}
import com.neo.sk.NiceBeat.core.UserManager
import com.neo.sk.NiceBeat.shared.ptcl.ErrorRsp

import scala.util.Random

/**
  * Created by hongruying on 2018/3/11
  */
trait HttpService
  extends ResourceService
    with ServiceUtils{

  import akka.actor.typed.scaladsl.AskPattern._
  import com.neo.sk.utils.CirceSupport._
  import io.circe.generic.auto._
  import io.circe._

  implicit val system: ActorSystem

  implicit val executor: ExecutionContextExecutor

  implicit val materializer: Materializer

  implicit val timeout: Timeout

  implicit val scheduler: Scheduler

  lazy val routes: Route = pathPrefix(AppSettings.rootPath){
    resourceRoutes ~
      (pathPrefix("game") & get){
        pathEndOrSingleSlash{
          getFromResource("html/admin.html")
        } ~
        path("join"){
          parameter(
            'name
          ){ name =>
            val flowFuture:Future[Flow[Message,Message,Any]] = userManager ? (UserManager.GetWebSocketFlow(name,_))
            dealFutureResult(
              flowFuture.map(t => handleWebSocketMessages(t))
            )
          }
        }
      }
  }
}
