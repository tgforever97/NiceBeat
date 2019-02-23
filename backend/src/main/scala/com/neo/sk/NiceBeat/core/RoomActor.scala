package com.neo.sk.NiceBeat.core

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors, StashBuffer, TimerScheduler}
import akka.http.scaladsl.model.ws.Message
import akka.stream.scaladsl.Flow
import com.neo.sk.NiceBeat.common.{AppSettings, Constants}
import com.neo.sk.NiceBeat.core.RoomManager.Command
import com.neo.sk.NiceBeat.core.game.GameContainerServerImpl
import com.neo.sk.NiceBeat.shared.model.Constants.GameState
import com.neo.sk.NiceBeat.shared.protocol.NBGameEvent
import org.slf4j.LoggerFactory
import concurrent.duration._
import scala.collection.mutable
import com.neo.sk.NiceBeat.Boot.executor
import org.seekloud.byteobject.MiddleBufferInJvm
/**
  * Created by hongruying on 2018/7/9
  * 管理房间砖块以及分发操作
  *
  *
  *
  */
object RoomActor {

  private val log = LoggerFactory.getLogger(this.getClass)

  private final val InitTime = Some(5.minutes)

  private final val classify= 100 // 10s同步一次状态

  private final val syncFrameOnly = 20 // 2s同步一次状态

  private final case object BehaviorChangeKey

  private final case object GameLoopKey

  sealed trait Command

  case class GetSyncState(uid:String) extends Command

  case class JoinRoom(uid: String, boardIdOpt: Option[Int], name: String, startTime: Long, userActor: ActorRef[UserActor.Command], roomId: Long) extends Command

  case class WebSocketMsg(uid: String, boardId: Int, req: NBGameEvent.UserActionEvent) extends Command with RoomManager.Command

  case class LeftRoom(uid: String, uName:String, boardId: Int, uidSet: List[(String, String)], roomId: Long) extends Command with RoomManager.Command

  case class LeftRoomByKilled(uid: String, name: String, boardId: Int) extends Command with RoomManager.Command

  final case class ChildDead[U](name: String, childRef: ActorRef[U]) extends Command with RoomManager.Command

  case object GameLoop extends Command

  case class TankRelive(userId: String, tankIdOpt: Option[Int], name: String) extends Command

  final case class SyncFrame4NewComer(userId:String,tickCount:Int) extends Command


  final case class SwitchBehavior(
                                   name: String,
                                   behavior: Behavior[Command],
                                   durationOpt: Option[FiniteDuration] = None,
                                   timeOut: TimeOut = TimeOut("busy time error")
                                 ) extends Command

  case class TimeOut(msg: String) extends Command

  private[this] def switchBehavior(ctx: ActorContext[Command],
                                   behaviorName: String, behavior: Behavior[Command], durationOpt: Option[FiniteDuration] = None, timeOut: TimeOut = TimeOut("busy time error"))
                                  (implicit stashBuffer: StashBuffer[Command],
                                   timer: TimerScheduler[Command]) = {
    log.debug(s"${ctx.self.path} becomes $behaviorName behavior.")
    timer.cancel(BehaviorChangeKey)
    durationOpt.foreach(timer.startSingleTimer(BehaviorChangeKey, timeOut, _))
    stashBuffer.unstashAll(ctx, behavior)
  }

  def create(roomId: Long): Behavior[Command] = {
    log.debug(s"Room Actor-${roomId} start...")
    Behaviors.setup[Command] {
      ctx =>
        Behaviors.withTimers[Command] {
          implicit timer =>
            val subscribersMap = mutable.HashMap[String, ActorRef[UserActor.Command]]()
            implicit val sendBuffer = new MiddleBufferInJvm(81920)
            val gameContainer = GameContainerServerImpl(AppSettings.niceBeatConfig, ctx.self, timer, log,
              dispatch(subscribersMap),
              dispatchTo(subscribersMap)
            )
            timer.startPeriodicTimer(GameLoopKey, GameLoop, (gameContainer.config.frameDuration / gameContainer.config.playRate).millis)
            idle(0l, roomId, Nil, Nil, mutable.HashMap[Long, Set[String]](), subscribersMap, gameContainer, 0L)
        }
    }
  }

  def idle(
            index: Long,
            roomId: Long,
            justJoinUser: List[(String, Option[Int], Long, ActorRef[UserActor.Command])],
            userMap: List[(String, Option[Int], String, Long, Long)],
            userGroup: mutable.HashMap[Long, Set[String]],
            subscribersMap: mutable.HashMap[String, ActorRef[UserActor.Command]],
            gameContainer: GameContainerServerImpl,
            tickCount: Long
          )(
            implicit timer: TimerScheduler[Command],
            sendBuffer: MiddleBufferInJvm
          ): Behavior[Command] = {
    Behaviors.receive { (ctx, msg) =>
      msg match {
        case JoinRoom(uid, boardIdOpt, name, startTime, userActor, roomId) =>
          gameContainer.joinGame(uid, name, userActor)
          //这一桢结束时会告诉所有新加入用户的tank信息以及地图全量数据
          userGroup.get(index%classify) match {
            case Some(s)=> userGroup.update(index%classify,s+uid)
            case None => userGroup.put(index%classify,Set(uid))
          }
          idle(index + 1, roomId, (uid, boardIdOpt, startTime, userActor) :: justJoinUser, (uid, boardIdOpt, name, startTime, index%classify) :: userMap,userGroup, subscribersMap, gameContainer, tickCount)

        case WebSocketMsg(uid, boardId, req) =>
          gameContainer.receiveUserAction(req)
          Behaviors.same

        case LeftRoom(uid, name, boardId, uidSet, roomId) =>
          subscribersMap.remove(uid)
          gameContainer.leftGame(uid, name, boardId)
          userMap.filter(_._1==uid).foreach{u=>
            userGroup.get(u._5) match {
              case Some(s)=> userGroup.update(u._5,s-u._1)
              case None=> userGroup.put(u._5,Set.empty)
            }
          }

          if (uidSet.isEmpty) {
            if (roomId > 1l) {
              Behaviors.stopped
            } else {
              idle(index,roomId, justJoinUser.filter(_._1 != uid), userMap.filter(_._1 != uid),userGroup, subscribersMap, gameContainer, tickCount)
            }
          } else {
            idle(index,roomId, justJoinUser.filter(_._1 != uid), userMap.filter(_._1 != uid), userGroup,subscribersMap, gameContainer, tickCount)
          }

        case LeftRoomByKilled(uid, boardId, name) =>
          log.debug("LeftRoomByKilled")
          subscribersMap.remove(uid)
          userMap.filter(_._1==uid).foreach{u=>
            userGroup.get(u._5) match {
              case Some(s)=> userGroup.update(u._5,s-u._1)
              case None=> userGroup.put(u._5,Set.empty)
            }
          }
          idle(index,roomId, justJoinUser.filter(_._1 != uid), userMap.filter(_._1 != uid), userGroup, subscribersMap, gameContainer, tickCount)


        case GameLoop =>

          gameContainer.update()
          //remind 错峰发送
          val state = gameContainer.getGameContainerState()
          userGroup.get(tickCount%classify).foreach{s=>
            if(s.nonEmpty){
              dispatch(subscribersMap.filter(r=>s.contains(r._1)))(NBGameEvent.SyncGameState(state))
            }
          }
          for(i <- (tickCount % syncFrameOnly) * (classify / syncFrameOnly) until (tickCount % syncFrameOnly + 1) * (classify / syncFrameOnly)){
            userGroup.get(i).foreach{s =>
              if(s.nonEmpty){
                dispatch(subscribersMap.filter(r=>s.contains(r._1)))(NBGameEvent.SyncGameState(gameContainer.getGameContainerState(true)))
              }
            }
          }
          if(justJoinUser.nonEmpty){
            val gameContainerAllState = gameContainer.getGameContainerAllState()
            justJoinUser.foreach { t =>
              subscribersMap.put(t._1, t._4)
              dispatchTo(subscribersMap)(t._1, NBGameEvent.SyncGameAllState(gameContainerAllState))
              timer.startSingleTimer(s"newComer${t._1}",SyncFrame4NewComer(t._1,1),100.millis)

            }
          }
          idle(index,roomId, Nil, userMap,userGroup, subscribersMap, gameContainer, tickCount + 1)

        case SyncFrame4NewComer(userId, tickCount) =>
          if(tickCount < 21){//新加入的玩家前2s高频率同步帧号
            dispatchTo(subscribersMap)(userId, NBGameEvent.SyncGameState(gameContainer.getGameContainerState(true)))
            timer.startSingleTimer(s"newComer$userId",SyncFrame4NewComer(userId,tickCount + 1),100.millis)
          }
          Behaviors.same

        case ChildDead(name, childRef) =>
          ctx.unwatch(childRef)
          Behaviors.same

        case m:GetSyncState=>
          val state = gameContainer.getGameContainerState()
          dispatchOnlyTo(subscribersMap, m.uid, NBGameEvent.SyncGameState(state))
          Behaviors.same

        case _ =>
          log.warn(s"${ctx.self.path} recv a unknow msg=${msg}")
          Behaviors.same


      }
    }

  }

  import scala.language.implicitConversions
  import org.seekloud.byteobject.ByteObject._

  def dispatch(subscribers: mutable.HashMap[String, ActorRef[UserActor.Command]])(msg: NBGameEvent.WsMsgServer)(implicit sendBuffer: MiddleBufferInJvm) = {
    val isKillMsg = msg.isInstanceOf[NBGameEvent.YouAreKilled]
    val isWinMsg = msg.isInstanceOf[NBGameEvent.YouWin]
    subscribers.values.foreach(_ ! UserActor.DispatchMsg(NBGameEvent.Wrap(msg.asInstanceOf[NBGameEvent.WsMsgServer].fillMiddleBuffer(sendBuffer).result(), isKillMsg, isWinMsg)))
  }

  def dispatchTo(subscribers: mutable.HashMap[String, ActorRef[UserActor.Command]])(id: String, msg: NBGameEvent.WsMsgServer)(implicit sendBuffer: MiddleBufferInJvm) = {
    val isKillMsg = msg.isInstanceOf[NBGameEvent.YouAreKilled]
    val isWinMsg = msg.isInstanceOf[NBGameEvent.YouWin]
    subscribers.get(id).foreach(_ ! UserActor.DispatchMsg(NBGameEvent.Wrap(msg.asInstanceOf[NBGameEvent.WsMsgServer].fillMiddleBuffer(sendBuffer).result(), isKillMsg, isWinMsg)))
  }

  def dispatchOnlyTo(subscribers: mutable.HashMap[String, ActorRef[UserActor.Command]], id: String, msg: NBGameEvent.WsMsgServer)(implicit sendBuffer: MiddleBufferInJvm) = {
    subscribers.get(id).foreach(_ ! UserActor.DispatchMsg(NBGameEvent.Wrap(msg.asInstanceOf[NBGameEvent.WsMsgServer].fillMiddleBuffer(sendBuffer).result(), false)))
  }

}
