package com.neo.sk.NiceBeat.core

import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors, StashBuffer, TimerScheduler}
import com.neo.sk.NiceBeat.core.RoomActor.{WebSocketMsg}
import com.neo.sk.NiceBeat.core.UserActor.{ JoinRoom, TimeOut}
import org.slf4j.LoggerFactory
import com.neo.sk.NiceBeat.common.AppSettings.personLimit
import scala.collection.mutable
import akka.actor.typed.Behavior


object RoomManager {
  private val log = LoggerFactory.getLogger(this.getClass)

  trait Command
  private case class TimeOut(msg:String) extends Command
  private case class ChildDead[U](name:String,childRef:ActorRef[U]) extends Command
  case class LeftRoom(uid:String,uName:String,boardId:Int) extends Command
//  case class LeftRoomByKilled(uid: String, uName: String, boardId: Int) extends Command


  def create():Behavior[Command] = {
    log.debug(s"RoomManager start...")
    Behaviors.setup[Command]{
      ctx =>
        implicit val stashBuffer = StashBuffer[Command](Int.MaxValue)
        Behaviors.withTimers[Command]{implicit timer =>
          val roomIdGenerator = new AtomicLong(1L)
          val roomInUse = mutable.HashMap((1l,List.empty[(String,String)]))
          idle(roomIdGenerator,roomInUse)
        }
    }
  }

  def idle(roomIdGenerator:AtomicLong,
           roomInUse:mutable.HashMap[Long,List[(String,String)]])
          (implicit stashBuffer: StashBuffer[Command],timer:TimerScheduler[Command]) = {
    Behaviors.receive[Command]{(ctx,msg) =>
      msg match {
        case JoinRoom(uid,tankIdOpt,name,startTime,userActor) =>
          roomInUse.find(p => p._2.length < personLimit).toList.sortBy(_._1).headOption match{
            case Some(t) =>
              roomInUse.put(t._1,(uid,name) :: t._2)
              getRoomActor(ctx,t._1) ! RoomActor.JoinRoom(uid,tankIdOpt,name,startTime,userActor,t._1)
            case None =>
              var roomId = roomIdGenerator.getAndIncrement()
              while(roomInUse.exists(_._1 == roomId))roomId = roomIdGenerator.getAndIncrement()
              roomInUse.put(roomId,List((uid,name)))
              getRoomActor(ctx,roomId) ! RoomActor.JoinRoom(uid,tankIdOpt,name,startTime,userActor,roomId)
          }
//          roomInUse.filter(r => r._2.length == 2).foreach{t =>
//            getRoomActor(ctx,t._1) ! RoomActor.ReachPersonLimit
//          }
          log.debug(s"now roomInUse:$roomInUse")
          Behaviors.same

        case LeftRoom(uid,uName,boardId) =>
          roomInUse.find(_._2.exists(_._1 == uid)) match{
            case Some(t) =>
              roomInUse.put(t._1,t._2.filterNot(_._1 == uid))
              getRoomActor(ctx,t._1) ! RoomActor.LeftRoom(uid,uName,boardId,roomInUse(t._1),t._1)
              if(roomInUse(t._1).isEmpty && t._1 > 1l) roomInUse.remove(t._1)
              log.debug(s"玩家：${uid}--remember to come back!!!$roomInUse")
            case None => log.debug(s"该玩家不在任何房间")
          }
          Behaviors.same

//        case LeftRoomByKilled(uid,name,boardId) =>
//          roomInUse.find(_._2.exists(_._1 == uid)) match{
//            case Some(t) =>
//              roomInUse.put(t._1,t._2.filterNot(_._1 == uid))
//              getRoomActor(ctx,t._1) ! RoomActor.LeftRoomByKilled(uid,name,boardId)
//              log.debug(s"${ctx.self.path}房间管理正在维护的信息${roomInUse}")
//            case None =>log.debug(s"this user doesn't exist")
//          }
//          Behaviors.same

        case ChildDead(child,childRef) =>
          log.debug(s"roomManager 不再监管room:$child,$childRef")
          ctx.unwatch(childRef)
          Behaviors.same

        case unknow =>
          Behaviors.same
      }
    }
  }

  private def getRoomActor(ctx:ActorContext[Command],roomId:Long) = {
    val childName = s"room_$roomId"
    ctx.child(childName).getOrElse{
      val actor = ctx.spawn(RoomActor.create(roomId),childName)
      ctx.watchWith(actor,ChildDead(childName,actor))
      actor

    }.upcast[RoomActor.Command]
  }

}
