package com.neo.sk.NiceBeat.shared.protocol

import com.neo.sk.NiceBeat.shared.`object`._
import com.neo.sk.NiceBeat.shared.config.{NiceBeatConfig, NiceBeatConfigImpl}
import com.neo.sk.NiceBeat.shared.model.{Point, Score}
import com.neo.sk.NiceBeat.shared.protocol.NBGameEvent.{UserEvent, WsMsgServer}

/**
  * Created by hongruying on 2018/8/28
  */
object NBGameEvent {

  final case class GameContainerAllState(
                                          f:Long,
                                          board:List[BoardState],
                                          ball:List[BallState],
                                          bricks:List[BrickState],
                                          boardMoveAction:List[(Int,Option[List[Byte]])]
                                        )

  case class GameContainerState(
                                 f:Long,
                                 board:Option[List[BoardState]],
                                 boardMoveAction:Option[List[(Int,Option[List[Byte]])]]
                               )

  /**前端建立WebSocket*/
  sealed trait WsMsgFrontSource
  case object CompleteMsgFrontServer extends WsMsgFrontSource
  case class FailMsgFrontServer(ex: Exception) extends WsMsgFrontSource

  sealed trait WsMsgFront extends WsMsgFrontSource
  /**
    * 携带原来Id
    * */
  final case class RestartGame(name:String) extends WsMsgFront
  final case object GetSyncGameState extends WsMsgFront

  /**后台建立WebSocket*/
  trait WsMsgSource
  case object CompleteMsgServer extends WsMsgSource
  case class FailMsgServer(ex: Exception) extends WsMsgSource

  sealed trait WsMsgServer extends WsMsgSource
  final case class EventData(list:List[WsMsgServer]) extends WsMsgServer
  final case class DecodeError() extends WsMsgServer

  final case class WsMsgErrorRsp(errCode:Int, msg:String) extends WsMsgServer
  final case class WsSuccess(userName:String) extends WsMsgServer
  final case class StartGame(userName:String) extends WsMsgFront
  final case class YourInfo(userId:String,userName:String, boardId:Int,ballId:Int,config:NiceBeatConfigImpl) extends WsMsgServer
  final case class ReachPersonLimit(config:NiceBeatConfigImpl) extends WsMsgServer

  @deprecated
  //用户胜利失败消息
  final case class YouAreKilled(boardId:Int, name:String) extends WsMsgServer
  final case class YouWin() extends WsMsgServer
  @deprecated
  final case class Ranks(currentRank: List[Score], historyRank: List[Score] = Nil) extends WsMsgServer
  final case class SyncGameState(state:GameContainerState) extends WsMsgServer
  final case class SyncGameAllState(gState:GameContainerAllState) extends WsMsgServer
  final case class Wrap(ws:Array[Byte],isKillMsg:Boolean = false,isWinMsg:Boolean = false) extends WsMsgSource
  final case class PingPackage(sendTime:Long) extends WsMsgServer with WsMsgFront

  sealed trait GameEvent {
    val frame:Long
  }

  trait UserEvent extends GameEvent
  trait EnvironmentEvent extends GameEvent  //游戏环境产生事件
  trait FollowEvent extends GameEvent  //游戏逻辑产生事件
  trait UserActionEvent extends UserEvent{   //游戏用户动作事件
    val boardId:Int
    val serialNum:Byte
  }
  /**异地登录消息
    * WebSocket连接重新建立*/
  final case object RebuildWebSocket extends WsMsgServer


  /**
    * 用户加入房间离开房间事件
    */

  final case class UserJoinRoom(userId:String, name:String, boardState:BoardState, ballState: BallState, override val frame: Long) extends UserEvent with WsMsgServer
  final case class UserRelive(userId:String, name:String, boardState: BoardState, ballState: BallState, override val frame:Long) extends UserEvent with WsMsgServer
  final case class UserLeftRoomByKill(userId:String, name:String, tankId:Int, override val frame:Long) extends UserEvent with WsMsgServer
  final case class UserLeftRoom(userId:String, name:String, boardId:Int, override val frame:Long) extends UserEvent with WsMsgServer

  /**
    * 用户按键操作事件
    */
  final case class UserPressKeyDown(boardId:Int,override val frame:Long,keyCodeDown:Byte,override val serialNum:Byte) extends UserActionEvent with WsMsgFront with WsMsgServer
  final case class UserPressKeyUp(boardId:Int,override val frame:Long,keyCodeUp:Byte,override val serialNum:Byte) extends UserActionEvent with WsMsgFront with WsMsgServer

  /**砖块事件*/
  final case class BrickRemove(brickId:Int, brickPos:Point, boardId:Int, override val frame:Long) extends EnvironmentEvent with WsMsgServer
  final case class BrickGenerate(brick:BrickState, override val frame:Long) extends EnvironmentEvent with WsMsgServer
  final case class BrickAttacked(brickId:Int, ballId:Int, damage:Int, override val frame:Long) extends FollowEvent

  /**
    * 碰撞消息
    */
  final case class CollisionEvent(ballId:Int, ballDirection:Float,override val frame:Long) extends WsMsgServer with FollowEvent

  /**
    * 砖块效果
    */
  final case class BrickFrozen(brickId:Int,attackedBoardId:Int,override val frame:Long) extends WsMsgServer with EnvironmentEvent
  final case class BrickIncrease(brickId:Int,attackedBoardId:Int,override val frame:Long) extends WsMsgServer with EnvironmentEvent

}
