package com.neo.sk.NiceBeat.core.game

import java.util.concurrent.atomic.AtomicInteger

import akka.actor.typed.ActorRef
import akka.actor.typed.scaladsl.TimerScheduler
import com.neo.sk.NiceBeat.core.{RoomActor, UserActor}
import com.neo.sk.NiceBeat.shared.`object`._
import com.neo.sk.NiceBeat.shared.config.{NiceBeatConfig, NiceBeatConfigImpl}
import com.neo.sk.NiceBeat.shared.game.GameContainer
import com.neo.sk.NiceBeat.shared.model.Constants.{BallDirection, BoardColor}
import com.neo.sk.NiceBeat.shared.model.{Point, Score}
import com.neo.sk.NiceBeat.shared.protocol.NBGameEvent
import com.neo.sk.NiceBeat.shared.protocol.NBGameEvent._
import org.slf4j.Logger

import scala.collection.mutable
import scala.util.Random



/**
  * Created by hongruying on 2018/8/29
  */
case class GameContainerServerImpl(
                                    config: NiceBeatConfig,
                                    roomActorRef: ActorRef[RoomActor.Command],
                                    timer: TimerScheduler[RoomActor.Command],
                                    log: Logger,
                                    dispatch: NBGameEvent.WsMsgServer => Unit,
                                    dispatchTo: (String, NBGameEvent.WsMsgServer) => Unit
                                  ) extends GameContainer {

  import scala.language.implicitConversions

  private val ballIdGenerator = new AtomicInteger(100)
  private val boardIdGenerator = new AtomicInteger(100)
  private val brickIdGenerator = new AtomicInteger(100)
  private var justJoinUser: List[(String, String, ActorRef[UserActor.Command])] = Nil
  private var brickPosList = List.empty[Point]
  private val brickPosInUse = mutable.Map[Int,List[Point]]()


  override def debug(msg: String): Unit = log.debug(msg)

  override def info(msg: String): Unit = log.info(msg)

  init()

  private def generateBrickPosList = {
    config.brickInitPosListX.flatMap{a =>
      config.brickInitPosListY.map{b =>
        Point(a,b)
      }
    }
  }

  private def genBrickPositionRandom(boardId:Int): Point = {
    val posForThisBoard = brickPosInUse.getOrElse(boardId,List.empty[Point])
    val pos = brickPosList.find(p => !posForThisBoard.contains(p)).head
    brickPosInUse(boardId) = posForThisBoard :+ pos
    pos
  }

  private def generateOneBrick(boardId:Int):Brick = {
    val bId = brickIdGenerator.getAndIncrement()
    val position = genBrickPositionRandom(boardId)
    Brick(config, bId, boardId, config.brickBlood, position, BoardColor.getRandomColorType(random))
  }

  private def chooseTheLuckyBoard:Int = {
    random.nextInt(boardMap.size)
  }

  override protected def handleBrickRemove(e:BrickRemove) = {
    super.handleBrickRemove(e)
    brickPosInUse(e.boardId) = brickPosInUse(e.boardId).filter(p => p != e.brickPos)
  }

  override protected def handleBrickAttacked(e: NBGameEvent.BrickAttacked): Unit = {
    brickMap.get(e.brickId).foreach { brick =>
      brick.attackedDamage(e.damage)
      if (!brick.isLived){
        brickMap.remove(e.brickId)
        //砖块消失
        val event = NBGameEvent.BrickRemove(e.brickId,brick.getBrickState().position,brick.boardId,systemFrame)
        dispatch(event)
        addGameEvent(event)
        val impactByteOpt = if(random.nextInt(10) < 5) Some(random.nextInt(2).toByte + 1) else None
        impactByteOpt match {
          case Some(1) =>
            val event = NBGameEvent.BrickFrozen(e.brickId,chooseTheLuckyBoard,systemFrame)
            dispatch(event)
            addGameEvent(event)
          case Some(2) =>
            val event = NBGameEvent.BrickIncrease(e.brickId,chooseTheLuckyBoard,systemFrame)
            dispatch(event)
            addGameEvent(event)
          case None =>

        }
      }
    }
  }

  private def generateBoard(userId: String, userName: String) = {
    val boardId = boardIdGenerator.getAndIncrement()
    Board(config,userId,userName,boardId,BoardColor.getRandomColorType(random),1,config.getBoardSpeed,0,config.getBoardInitPosition,false)
  }

  private def generateBall(boardId:Int) = {
    val ballId = ballIdGenerator.getAndIncrement()
    val angle = BallDirection.directionListUp(BallDirection.getRandomDirection(random))
    val direction = -angle*Pi/180
    Ball(config,ballId,boardId,direction,config.getBallSpeed,config.getBallInitPosition,config.getBallDamage(1),true)
  }

  override def handleGameStart = {
    super.handleGameStart
  }

  override protected def handleUserJoinRoomEventNow() = {
      justJoinUser.foreach {
        case (userId, name, ref) =>
          val board = generateBoard(userId,name)
          val ball =  generateBall(board.boardId)
          val event = NBGameEvent.UserJoinRoom(userId,name,board.getBoardState(),ball.getBallState(),systemFrame)
          dispatch(event)
          addGameEvent(event)
          ref ! UserActor.JoinRoomSuccess(board,ball,config.getNiceBeatConfigImpl(),userId,name,roomActor = roomActorRef)
          boardMap.put(board.boardId,board)
          ballMap.put(ball.bId,ball)
          (0 until config.brickNum).foreach{i =>
            val brick = generateOneBrick(board.boardId)
            brickMap.put(brick.bId,brick)
          }
      }
      justJoinUser = Nil
  }


  def leftGame(userId: String, name: String, boardId: Int) = {
    val event = NBGameEvent.UserLeftRoom(userId,name,boardId,systemFrame)
    addGameEvent(event)
    dispatch(event)
  }

  def joinGame(userId: String, name: String, userActor: ActorRef[UserActor.Command]): Unit = {
    justJoinUser = (userId, name, userActor) :: justJoinUser
  }

  def receiveUserAction(preExecuteUserAction: NBGameEvent.UserActionEvent): Unit = {
    val f = math.max(preExecuteUserAction.frame, systemFrame)
    if (preExecuteUserAction.frame != f) {
      log.debug(s"preExecuteUserAction fame=${preExecuteUserAction.frame}, systemFrame=${systemFrame}")
    }
    val action = preExecuteUserAction match {
      case a: NBGameEvent.UserPressKeyDown => a.copy(frame = f)
      case a: NBGameEvent.UserPressKeyUp => a.copy(frame = f)
    }
    addUserAction(action)
    dispatch(action)
  }

  private def init(): Unit = {
    brickPosList = generateBrickPosList
    clearEventWhenUpdate()
  }

  override protected def clearEventWhenUpdate(): Unit = {
    gameEventMap -= systemFrame - 1
    actionEventMap -= systemFrame - 1
    systemFrame += 1
  }

  override def update(): Unit = {
    super.update()
  }

  override def deadBallCallBack(ball:Ball) = {
    super.deadBallCallBack(ball)
    val board = boardMap.get(ball.boardId)
    board match{
      case Some(b) =>
        val event = NBGameEvent.YouAreKilled(b.boardId,b.name)
        dispatchTo(b.userId,event)
      case None =>
        log.info(s"there is no borad ${ball.boardId}")
    }
  }


  def getGameContainerState(frameOnly:Boolean = false): GameContainerState = {
    GameContainerState(
      systemFrame,
      if(frameOnly) None else Some(boardMap.values.map(_.getBoardState()).toList),
      if(frameOnly) None else Some(boardMoveAction.toList.map(t => (t._1,if(t._2.isEmpty) None else Some(t._2.toList))))
    )
  }

  def getLastGameEvent(): List[NBGameEvent.WsMsgServer] = {
    (gameEventMap.getOrElse(this.systemFrame - 1, Nil) ::: actionEventMap.getOrElse(this.systemFrame - 1, Nil))
      .filter(_.isInstanceOf[NBGameEvent.WsMsgServer]).map(_.asInstanceOf[NBGameEvent.WsMsgServer])
  }

  override protected implicit def boardState2Impl(board: BoardState): Board = {
    new Board(config,board)
  }

  override protected implicit def ballState2Impl(ball: BallState): Ball = {
    new Ball(config,ball)
  }


}
