package com.neo.sk.NiceBeat.core

import akka.actor.Terminated
import akka.actor.typed.scaladsl.{ActorContext, Behaviors, StashBuffer, TimerScheduler}
import akka.actor.typed.{ActorRef, Behavior}
import akka.stream.OverflowStrategy
import akka.stream.scaladsl.Flow
import akka.stream.typed.scaladsl.{ActorSink, ActorSource}
import com.neo.sk.NiceBeat.common.{AppSettings, Constants}
import com.neo.sk.NiceBeat.core.game.NiceBeatConfigServerImpl
import com.neo.sk.NiceBeat.shared.model.Constants.GameState
import org.seekloud.byteobject.MiddleBufferInJvm
import com.neo.sk.NiceBeat.shared.protocol.NBGameEvent.CompleteMsgServer
import com.neo.sk.NiceBeat.shared.ptcl.ErrorRsp
import org.slf4j.LoggerFactory
import com.neo.sk.NiceBeat.Boot.roomManager
import com.neo.sk.NiceBeat.shared.`object`.{Ball, Board}
import com.neo.sk.NiceBeat.shared.config.NiceBeatConfigImpl
import com.neo.sk.NiceBeat.shared.protocol.NBGameEvent

import scala.concurrent.duration._
import scala.language.implicitConversions
import org.seekloud.byteobject.ByteObject._
/**
  * Created by hongruying on 2018/7/9
  *
  */
object UserActor {

  private val log = LoggerFactory.getLogger(this.getClass)

  private final val InitTime = Some(5.minutes)
  private final case object BehaviorChangeKey

  trait Command

  case class WebSocketMsg(reqOpt:Option[NBGameEvent.WsMsgFront]) extends Command

  case object CompleteMsgFront extends Command
  case class FailMsgFront(ex: Throwable) extends Command

  /**此消息用于外部控制状态转入初始状态，以便于重建WebSocket*/
  case object ChangeBehaviorToInit extends Command

  case class UserFrontActor(actor:ActorRef[NBGameEvent.WsMsgSource]) extends Command

  case class DispatchMsg(msg:NBGameEvent.WsMsgSource) extends Command
  case class WsSuccess(userName:String) extends Command
  case class StartGame(userName:String) extends Command

  case class JoinRoom(uid:String,tankIdOpt:Option[Int],name:String,startTime:Long,userActor:ActorRef[UserActor.Command]) extends Command with RoomManager.Command
  case class JoinRoomSuccess(board:Board,ball:Ball,config:NiceBeatConfigImpl,uId:String,uName:String,roomActor: ActorRef[RoomActor.Command]) extends Command with RoomManager.Command
  case class JoinRoomFail(msg:String) extends Command
  case class UserLeft[U](actorRef:ActorRef[U]) extends Command
  case class JoinGame(userName:String) extends Command

  final case class SwitchBehavior(
                                   name: String,
                                   behavior: Behavior[Command],
                                   durationOpt: Option[FiniteDuration] = None,
                                   timeOut: TimeOut = TimeOut("busy time error")
                                 ) extends Command

  case class TimeOut(msg:String) extends Command

  private[this] def switchBehavior(ctx: ActorContext[Command],
                                   behaviorName: String, behavior: Behavior[Command], durationOpt: Option[FiniteDuration] = None,timeOut: TimeOut  = TimeOut("busy time error"))
                                  (implicit stashBuffer: StashBuffer[Command],
                                   timer:TimerScheduler[Command]) = {
    log.debug(s"${ctx.self.path} becomes $behaviorName behavior.")
    timer.cancel(BehaviorChangeKey)
    durationOpt.foreach(timer.startSingleTimer(BehaviorChangeKey,timeOut,_))
    stashBuffer.unstashAll(ctx,behavior)
  }


  private def sink(actor: ActorRef[Command]) = ActorSink.actorRef[Command](
    ref = actor,
    onCompleteMessage = CompleteMsgFront,
    onFailureMessage = FailMsgFront.apply
  )

  def flow(actor:ActorRef[UserActor.Command]):Flow[WebSocketMsg,NBGameEvent.WsMsgSource,Any] = {
    val in = Flow[WebSocketMsg].to(sink(actor))
    val out =
      ActorSource.actorRef[NBGameEvent.WsMsgSource](
        completionMatcher = {
          case NBGameEvent.CompleteMsgServer ⇒
        },
        failureMatcher = {
          case NBGameEvent.FailMsgServer(e)  ⇒ e
        },
        bufferSize = 128,
        overflowStrategy = OverflowStrategy.dropHead
      ).mapMaterializedValue(outActor => actor ! UserFrontActor(outActor))
    Flow.fromSinkAndSource(in, out)
  }


  def create(uId:String):Behavior[Command] = {
    Behaviors.setup[Command]{ctx =>
      log.debug(s"${ctx.self.path} is starting...")
      implicit val stashBuffer = StashBuffer[Command](Int.MaxValue)
      Behaviors.withTimers[Command] { implicit timer =>
        implicit val sendBuffer = new MiddleBufferInJvm(8192)
        switchBehavior(ctx,"init",init(uId),InitTime,TimeOut("init"))
      }
    }
  }

  private def init(uId:String)(
    implicit stashBuffer:StashBuffer[Command],
    sendBuffer:MiddleBufferInJvm,
    timer:TimerScheduler[Command]
  ): Behavior[Command] =
    Behaviors.receive[Command] { (ctx, msg) =>
      msg match {
        case UserFrontActor(frontActor) =>
          ctx.watchWith(frontActor,UserLeft(frontActor))
          switchBehavior(ctx,"idle",idle(uId, System.currentTimeMillis(), frontActor))

        case UserLeft(actor) =>
          ctx.unwatch(actor)
          Behaviors.stopped

        case TimeOut(m) =>
          log.debug(s"${ctx.self.path} is time out when busy,msg=${m}")
          Behaviors.stopped

        case unknowMsg =>
          stashBuffer.stash(unknowMsg)
          Behavior.same
      }
    }


  private def idle(uId:String, startTime:Long, frontActor:ActorRef[NBGameEvent.WsMsgSource])(
    implicit stashBuffer:StashBuffer[Command],
    timer:TimerScheduler[Command],
    sendBuffer:MiddleBufferInJvm
  ): Behavior[Command] =
    Behaviors.receive[Command] { (ctx, msg) =>
      msg match {

        case WsSuccess(name) =>
         frontActor ! NBGameEvent.Wrap(NBGameEvent.WsSuccess(name).asInstanceOf[NBGameEvent.WsMsgServer].fillMiddleBuffer(sendBuffer).result())
         Behaviors.same

        case JoinGame(uName)=>
          log.info("userActor joingame")
          roomManager ! JoinRoom(uId,None,uName,startTime,ctx.self)
          Behaviors.same

        case JoinRoomFail(msg) =>
          frontActor ! NBGameEvent.Wrap(NBGameEvent.WsMsgErrorRsp(10001, msg).asInstanceOf[NBGameEvent.WsMsgServer].fillMiddleBuffer(sendBuffer).result())
          Behaviors.same

        case JoinRoomSuccess(board,ball,config,`uId`,uName,roomActor) =>
          //给前端Actor同步当前桢数据，然后进入游戏Actor
          frontActor ! NBGameEvent.Wrap(NBGameEvent.YourInfo(uId,uName,board.boardId,ball.bId,config).asInstanceOf[NBGameEvent.WsMsgServer].fillMiddleBuffer(sendBuffer).result())
          switchBehavior(ctx,"play",play(uId,uName,board,ball,startTime,frontActor,roomActor))

        case WebSocketMsg(reqOpt) =>
          reqOpt match {
            case Some(t:NBGameEvent.StartGame) =>
              ctx.self ! JoinGame(t.userName)
              idle(uId,startTime,frontActor)
            case _ =>
              Behaviors.same
          }
        /**
          * 本消息内转换为初始状态并给前端发送异地登录消息*/
        case ChangeBehaviorToInit=>
          ctx.unwatch(frontActor)
          switchBehavior(ctx,"init",init(uId),InitTime,TimeOut("init"))

        case UserLeft(actor) =>
          ctx.unwatch(actor)
          switchBehavior(ctx,"init",init(uId),InitTime,TimeOut("init"))

        case unknowMsg =>
          log.warn(s"got unknown msg: $unknowMsg")
          Behavior.same
      }
    }

  private def play(
                    uId:String,
                    uName:String,
                    board:Board,
                    ball:Ball,
                    startTime:Long,
                    frontActor:ActorRef[NBGameEvent.WsMsgSource],
                    roomActor: ActorRef[RoomActor.Command])(
                    implicit stashBuffer:StashBuffer[Command],
                    timer:TimerScheduler[Command],
                    sendBuffer:MiddleBufferInJvm
                  ): Behavior[Command] =
    Behaviors.receive[Command] { (ctx, msg) =>
      msg match {
        case WebSocketMsg(reqOpt) =>
          if(reqOpt.nonEmpty){
            reqOpt.get match{
              case t:NBGameEvent.UserActionEvent =>
                roomActor ! RoomActor.WebSocketMsg(uId, board.boardId, t)
                Behaviors.same

              case t: NBGameEvent.PingPackage =>
                frontActor ! NBGameEvent.Wrap(t.asInstanceOf[NBGameEvent.WsMsgServer].fillMiddleBuffer(sendBuffer).result())
                Behaviors.same

              case NBGameEvent.GetSyncGameState =>
                roomActor ! RoomActor.GetSyncState(uId)
                Behaviors.same

              case t:NBGameEvent.RestartGame =>
                val newStartTime = System.currentTimeMillis()
                switchBehavior(ctx,"idle",idle(uId,newStartTime,frontActor))


              case _ =>
                Behaviors.same
            }
          }else{
            Behaviors.same
          }

        case DispatchMsg(m) =>
          if(m.asInstanceOf[NBGameEvent.Wrap].isKillMsg) {
            roomManager ! RoomManager.LeftRoomByKilled(uId,uName,board.boardId)
          }
          if(m.asInstanceOf[NBGameEvent.Wrap].isWinMsg) {
            roomManager ! RoomManager.LeftRoom(uId,uName,board.boardId)
          }
          frontActor ! m
          Behaviors.same

        case ChangeBehaviorToInit=>
          frontActor ! NBGameEvent.Wrap(NBGameEvent.RebuildWebSocket.asInstanceOf[NBGameEvent.WsMsgServer].fillMiddleBuffer(sendBuffer).result())
          roomManager ! RoomManager.LeftRoom(uId,uName,board.boardId)
          ctx.unwatch(frontActor)
          switchBehavior(ctx,"init",init(uId),InitTime,TimeOut("init"))

        case UserLeft(actor) =>
          ctx.unwatch(actor)
          roomManager ! RoomManager.LeftRoom(uId,uName,board.boardId)
          Behaviors.stopped

        case unknowMsg =>
          Behavior.same
      }
    }



  private def busy()(
    implicit stashBuffer:StashBuffer[Command],
    timer:TimerScheduler[Command]
  ): Behavior[Command] =
    Behaviors.receive[Command] { (ctx, msg) =>
      msg match {
        case SwitchBehavior(name, behavior,durationOpt,timeOut) =>
          switchBehavior(ctx,name,behavior,durationOpt,timeOut)

        case TimeOut(m) =>
          log.debug(s"${ctx.self.path} is time out when busy,msg=${m}")
          Behaviors.stopped

        case unknowMsg =>
          stashBuffer.stash(unknowMsg)
          Behavior.same
      }
    }

}
