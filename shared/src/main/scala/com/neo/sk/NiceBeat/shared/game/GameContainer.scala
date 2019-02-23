package com.neo.sk.NiceBeat.shared.game

import com.neo.sk.NiceBeat.shared.`object`._
import com.neo.sk.NiceBeat.shared.config.NiceBeatConfig
import com.neo.sk.NiceBeat.shared.model.Constants
import com.neo.sk.NiceBeat.shared.model.Constants.BallDirection
import com.neo.sk.NiceBeat.shared.model.{Point, Rectangle, Score}
import com.neo.sk.NiceBeat.shared.protocol.NBGameEvent
import com.neo.sk.NiceBeat.shared.protocol.NBGameEvent._

import scala.collection.mutable
import scala.util.Random

/**
  * Created by hongruying on 2018/8/22
  * Edited by wmy on 2018/02/13
  * 游戏逻辑的基类
  *
  * 逻辑帧更新逻辑：
  * 先处理玩家离开的游戏事件
  * 弹球的运动逻辑，障碍检测
  * 更新用户操作所影响弹板的状态
  * 伤害计算事件
  * 用户加入游戏事件的处理
  */

trait GameContainer{

  import scala.language.implicitConversions

  def debug(msg: String): Unit

  def info(msg: String): Unit

  implicit val config:NiceBeatConfig

  val boundary : Point = config.boundary
  var systemFrame:Long = 0L //系统帧数
  val boardMap = mutable.HashMap[Int,Board]() //boardId -> board
  var ballMap = mutable.HashMap[Int,Ball]() //ballId -> ball
  val brickMap = mutable.HashMap[Int,Brick]() //brickId -> brick
  val boardMoveAction = mutable.HashMap[Int,mutable.HashSet[Byte]]() //boardId -> pressed direction key code
  val random = new Random(System.currentTimeMillis())
  val Pi = math.Pi.toFloat

  protected val gameEventMap = mutable.HashMap[Long,List[GameEvent]]() //frame -> List[GameEvent] 待处理的事件 frame >= curFrame
  protected val actionEventMap = mutable.HashMap[Long,List[UserActionEvent]]() //frame -> List[UserActionEvent]
  protected val followEventMap = mutable.HashMap[Long,List[FollowEvent]]()  // 记录游戏逻辑中产生事件

  final protected def handleUserJoinRoomEvent(l:List[UserJoinRoom]) :Unit = {
    l foreach handleUserJoinRoomEvent
  }

  protected def handleUserJoinRoomEvent(e:UserJoinRoom) :Unit = {
    val board:Board = e.boardState
    val ball:Ball = e.ballState
    boardMap.put(e.boardState.boardId,board)
    ballMap.put(e.ballState.bId,ball)
  }

  protected implicit def boardState2Impl(board:BoardState):Board
  protected implicit def ballState2Impl(ball:BallState):Ball

  //服务器和客户端执行的逻辑不一致
  protected def handleUserJoinRoomEventNow() = {
    gameEventMap.get(systemFrame).foreach{ events =>
      handleUserJoinRoomEvent(events.filter(_.isInstanceOf[UserJoinRoom]).map(_.asInstanceOf[UserJoinRoom]).reverse)
    }
  }

//  protected def handleGameStart = {
//    if(boardMap.size == 2){
//      ballMap.foreach(ball =>
//      ball._2.changeMoveTrue)
//    }
//  }

  final protected def handleUserReliveEvent(l:List[UserRelive]):Unit = {
    l foreach handleUserReliveEvent
  }

  protected def handleUserReliveEvent(e:UserRelive):Unit = {
    val board = e.boardState
    if(!boardMap.exists(_._1 == board.boardId)){
      boardMap.put(board.boardId,board)
    }
    val ball = e.ballState
    if(!ballMap.exists(_._1 == ball.bId)){
      ballMap.put(ball.bId, ball)
    }
  }

  protected def handleUserReliveNow() = {
    gameEventMap.get(systemFrame).foreach{events =>
      handleUserReliveEvent(events.filter(_.isInstanceOf[UserRelive]).map(_.asInstanceOf[UserRelive]).reverse)
    }
  }

  protected final def handleUserLeftRoom(e:UserLeftRoom) :Unit = {
    boardMoveAction.remove(e.boardId)
    boardMap.remove(e.boardId)
    ballMap = ballMap.filterNot(b => b._2.boardId == e.boardId)
  }

  final protected def handleUserLeftRoom(l:List[UserLeftRoom]) :Unit = {
    l foreach handleUserLeftRoom
  }

  final protected def handleUserLeftRoomNow() = {
    gameEventMap.get(systemFrame).foreach{ events =>
      handleUserLeftRoom(events.filter(_.isInstanceOf[UserLeftRoom]).map(_.asInstanceOf[UserLeftRoom]).reverse)
    }
  }


  protected final def handleUserActionEvent(actions:List[UserActionEvent]) = {
    /**
      * 用户行为事件
      * */
    actions.sortBy(t => (t.boardId,t.serialNum)).foreach{ action =>
      val boardMoveSet = boardMoveAction.getOrElse(action.boardId,mutable.HashSet[Byte]())
      boardMap.get(action.boardId) match {
        case Some(board) =>
          action match {
            case a:UserPressKeyDown =>
              boardMoveSet.add(a.keyCodeDown)
              boardMoveAction.put(a.boardId,boardMoveSet)
              board.setBoardDirection(boardMoveSet.toSet)
            case a:UserPressKeyUp =>
              boardMoveSet.remove(a.keyCodeUp)
              boardMoveAction.put(a.boardId,boardMoveSet)
              board.setBoardDirection(boardMoveSet.toSet)
          }
        case None => info(s"boardId=${action.boardId} action=${action} is no valid,because the board is not exist")
      }
    }
  }

  final protected def handleUserActionEventNow() = {
    actionEventMap.get(systemFrame).foreach{ actionEvents =>
      handleUserActionEvent(actionEvents.reverse)
    }
  }

  protected def handleBrickAttacked(e:BrickAttacked) :Unit = {
    brickMap.get(e.brickId).foreach{brick =>
      if(brick.isLived){
        brick.attackedDamage(e.damage)
      }
    }
  }

  protected final def handleBrickAttacked(es:List[BrickAttacked]) :Unit = {
    es foreach handleBrickAttacked
  }

  final protected def handleBrickAttackedNow() = {
    followEventMap.get(systemFrame).foreach{ events =>
      handleBrickAttacked(events.filter(_.isInstanceOf[BrickAttacked]).map(_.asInstanceOf[BrickAttacked]).reverse)
    }
  }

  protected def handleGenerateBrick(e:BrickGenerate) :Unit = {
    val newBrick = Brick(config,e.brick.bId,e.brick.boardId,e.brick.blood,e.brick.position,e.brick.colorType)
    brickMap.put(newBrick.bId,newBrick)
  }

  protected final def handleGenerateBrick(es:List[BrickGenerate]) :Unit = {
    es foreach handleGenerateBrick
  }

  final protected def handleGenerateBrickNow() = {
    gameEventMap.get(systemFrame).foreach{ events =>
      handleGenerateBrick(events.filter(_.isInstanceOf[BrickGenerate]).map(_.asInstanceOf[BrickGenerate]).reverse)
    }
  }

  protected def handleBrickRemove(e:BrickRemove) :Unit = {
    brickMap.get(e.brickId).foreach{b =>
      brickMap.remove(b.bId)
    }
  }

  protected final def handleBrickRemove(es:List[BrickRemove]) :Unit = {
    es foreach handleBrickRemove
  }

  protected def handleBrickRemoveNow()={
    gameEventMap.get(systemFrame).foreach{events=>
      handleBrickRemove(events.filter(_.isInstanceOf[BrickRemove]).map(_.asInstanceOf[BrickRemove]).reverse)
    }
  }

  protected def boardMove():Unit = {
    boardMap.toList.sortBy(_._1).map(_._2).foreach{ board =>
      board.move(boundary)
    }
  }

  protected def ballMove():Unit = {
    ballMap.toList.sortBy(_._1).map(_._2).foreach{ ball =>
      println(s"ball ${ball.position}")
      println(s"${ball.direction}")
      val bricks = brickMap.filter(b => b._2.boardId == ball.boardId).values
      val board = boardMap(ball.boardId)
      bricks.foreach{t =>
          ball.checkCollisionObject(t,bricksCallBack(ball))}
      ball.checkCollisionObject(board,boardCallBack(ball))
      ball.checkCollisionWall(boundary,wallCallBack(ball))
      ball.move(boundary,deadBallCallBack)
    }
  }

  protected def boardCallBack(ball: Ball)(board: Board):Unit = {

    println(s"brick collision")
    val positionBoard = board.getBoardState().position
    val positionBall = ball.getBallState().position
//    ball.setBallPosition(positionBoard,positionBall,board.width,board.height)
    val angle = BallDirection.directionListUp(BallDirection.getRandomDirection(random))
    val direction = -angle*Pi/180
    ball.setBallDirection(direction)
  }

  protected def bricksCallBack(ball: Ball)(b:Brick):Unit = {

    println(s"brick collision")
    val positionBrick = b.getBrickState().position
    val positionBall = ball.getBallState().position
//    ball.setBallPosition(positionBrick,positionBall,b.width,b.height)
    val angle = BallDirection.directionListUp(BallDirection.getRandomDirection(random))
    val direction = angle*Pi/180
    ball.setBallDirection(direction)
    val event2 = NBGameEvent.BrickAttacked(b.bId,ball.bId,ball.damage,systemFrame)
    addFollowEvent(event2)
  }

  protected def wallCallBack(ball:Ball):Unit = {

    if(ball.getBallState().position.x + ball.radius == boundary.x){
      println(s"right wall collision")
      val angle = BallDirection.directionListLeft(BallDirection.getRandomDirection(random))
      val direction = angle*Pi/180
      ball.setBallDirection(direction)
    }
    else if(ball.getBallState().position.x - ball.radius == 0){
      println(s"left wall collision")
      val angle = BallDirection.directionListLeft(BallDirection.getRandomDirection(random))
      val direction = (180-angle)*Pi/180
      ball.setBallDirection(direction)
    }
    else if(ball.getBallState().position.y - ball.radius == 0){
      println(s"up wall collision")
      val angle = BallDirection.directionListUp(BallDirection.getRandomDirection(random))
      val direction = angle*Pi/180
      ball.setBallDirection(direction)
    }
  }

  protected def deadBallCallBack(ball: Ball):Unit = {
    println(s"remove ball")
    ballMap.remove(ball.bId)
  }

  protected final def objectMove():Unit = {
    boardMove()
    ballMove()
  }

  protected final def addUserAction(action:UserActionEvent):Unit = {
    actionEventMap.get(action.frame) match {
      case Some(actionEvents) => actionEventMap.put(action.frame,action :: actionEvents)
      case None => actionEventMap.put(action.frame,List(action))
    }
  }


  protected final def addGameEvent(event:GameEvent):Unit = {
    gameEventMap.get(event.frame) match {
      case Some(events) => gameEventMap.put(event.frame, event :: events)
      case None => gameEventMap.put(event.frame,List(event))
    }
  }

  protected final def addFollowEvent(event:FollowEvent):Unit = {
    followEventMap.get(event.frame) match {
      case Some(events) => followEventMap.put(event.frame, event :: events)
      case None => followEventMap.put(event.frame,List(event))
    }
  }

  //更新本桢的操作
  def update():Unit = {
    handleUserLeftRoomNow()
//    handleGameStart
    objectMove()
    handleUserActionEventNow()
    handleBrickAttackedNow()
    handleBrickRemoveNow()
    handleGenerateBrickNow()
    handleUserJoinRoomEventNow()
    handleUserReliveNow()
    clearEventWhenUpdate()
  }


  protected def clearEventWhenUpdate():Unit

  def getGameContainerAllState():GameContainerAllState = {
    GameContainerAllState(
      systemFrame,
      boardMap.values.map(_.getBoardState()).toList,
      ballMap.values.map(_.getBallState()).toList,
      brickMap.values.map(_.getBrickState()).toList,
      boardMoveAction.toList.map(t => (t._1,if(t._2.isEmpty) None else Some(t._2.toList)))
    )
  }

  protected def addGameEvents(frame:Long,events:List[GameEvent],actionEvents:List[UserActionEvent]) = {
    gameEventMap.put(frame,events)
    actionEventMap.put(frame,actionEvents)
  }

  def removePreEvent(frame:Long, boardId:Int, serialNum:Byte):Unit = {
    actionEventMap.get(frame).foreach{ actions =>
      actionEventMap.put(frame,actions.filterNot(t => t.boardId == boardId && t.serialNum == serialNum))
    }
  }

}