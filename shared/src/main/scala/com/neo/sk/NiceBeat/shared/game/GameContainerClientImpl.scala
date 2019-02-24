package com.neo.sk.NiceBeat.shared.game

import com.neo.sk.NiceBeat.shared.`object`._
import com.neo.sk.NiceBeat.shared.config.NiceBeatConfig
import com.neo.sk.NiceBeat.shared.game.view._
import com.neo.sk.NiceBeat.shared.model.Constants.{GameAnimation, GameState}
import com.neo.sk.NiceBeat.shared.model.{Point, Score}
import com.neo.sk.NiceBeat.shared.protocol.NBGameEvent
import com.neo.sk.NiceBeat.shared.protocol.NBGameEvent._
import com.neo.sk.NiceBeat.shared.util.canvas.{MiddleCanvas, MiddleContext, MiddleFrame}

import scala.collection.mutable


/**
  * Created by hongruying on 2018/8/24
  * 终端
  */
case class GameContainerClientImpl(
                                    drawFrame: MiddleFrame,
                                    ctx: MiddleContext,
                                    override val config: NiceBeatConfig,
                                    myId: String,
                                    myBoardId: Int,
                                    myBallId:Int,
                                    myName: String,
                                    var canvasSize: Point,
                                    var canvasUnit: Int
                                  ) extends GameContainer with EsRecover
  with BackgroundDrawUtil with BrickDrawUtil with FpsComponentsDrawUtil with BoardDrawUtil with InfoDrawUtil {

  import scala.language.implicitConversions

  protected val brickAttackedAnimationMap = mutable.HashMap[Int, Int]()
  var boardId:Int = myBoardId
  var ballId:Int = myBallId
  protected val myBoardMoveAction = mutable.HashMap[Long,List[UserActionEvent]]()

  override def debug(msg: String): Unit = {}

  override def info(msg: String): Unit = println(msg)

  private val esRecoverSupport: Boolean = true

  private val uncheckedActionMap = mutable.HashMap[Byte, Long]() //serinum -> frame

  private var gameContainerAllStateOpt: Option[GameContainerAllState] = None
  private var gameContainerStateOpt: Option[GameContainerState] = None

  protected var waitSyncData: Boolean = true

  private val preExecuteFrameOffset = com.neo.sk.NiceBeat.shared.model.Constants.PreExecuteFrameOffset

  def updateClientSize(canvasS: Point, cUnit: Int) = {
    canvasUnit = cUnit
    canvasSize = canvasS
    updateBackSize(canvasS)
    updateFpsSize(canvasS)
    updateBoardSize(canvasS)
  }

  override protected def handleBrickAttacked(e: NBGameEvent.BrickAttacked): Unit = {
    super.handleBrickAttacked(e)
    if (brickMap.get(e.brickId).nonEmpty){
      brickAttackedAnimationMap.put(e.brickId, GameAnimation.ballHitAnimationFrame)
    }
  }

  override protected implicit def boardState2Impl(board: BoardState): Board = {
    new Board(config, board)
  }

  override protected implicit def ballState2Impl(ball: BallState): Ball= {
    new Ball(config, ball)
  }

  def receiveGameEvent(e: GameEvent) = {
    if (e.frame >= systemFrame) {
      addGameEvent(e)
    } else if (esRecoverSupport) {
      println(s"rollback-frame=${e.frame},curFrame=${this.systemFrame},e=${e}")
      rollback4GameEvent(e)
    }
  }

  //接受服务器的用户事件
  def receiveUserEvent(e: UserActionEvent) = {
    if (e.boardId == boardId) {
      uncheckedActionMap.get(e.serialNum) match {
        case Some(preFrame) =>
          if (e.frame != preFrame) {
            println(s"preFrame=${preFrame} eventFrame=${e.frame} curFrame=${systemFrame}")
            if (preFrame < e.frame && esRecoverSupport) {
              if (preFrame >= systemFrame) {
                removePreEvent(preFrame, e.boardId, e.serialNum)
                addUserAction(e)
              } else if (e.frame >= systemFrame) {
                removePreEventHistory(preFrame, e.boardId, e.serialNum)
                rollback(preFrame)
                addUserAction(e)
              } else {
                removePreEventHistory(preFrame, e.boardId, e.serialNum)
                addUserActionHistory(e)
                rollback(preFrame)
              }
            }
          }
        case None =>
          if (e.frame >= systemFrame) {
            addUserAction(e)
          } else if (esRecoverSupport) {
            rollback4UserActionEvent(e)
          }
      }
    } else {
      if (e.frame >= systemFrame) {
        addUserAction(e)
      } else if (esRecoverSupport) {
        rollback4UserActionEvent(e)
      }
    }
  }

  def preExecuteUserEvent(action: UserActionEvent) = {
    addUserAction(action)
    uncheckedActionMap.put(action.serialNum, action.frame)
  }

  final def addMyAction(action: UserActionEvent): Unit = {
    if (action.boardId == boardId) {
      myBoardMoveAction.get(action.frame - preExecuteFrameOffset) match {
        case Some(actionEvents) => myBoardMoveAction.put(action.frame - preExecuteFrameOffset, action :: actionEvents)
        case None => myBoardMoveAction.put(action.frame - preExecuteFrameOffset, List(action))
      }
    }
  }

  override protected def handleUserJoinRoomEvent(e: NBGameEvent.UserJoinRoom): Unit = {
    super.handleUserJoinRoomEvent(e)
  }


  protected def handleGameContainerAllState(gameContainerAllState: GameContainerAllState) = {
    systemFrame = gameContainerAllState.f
    boardMap.clear()
    brickMap.clear()
    boardMoveAction.clear()
    ballMap.clear()
    waitSyncData = false

    gameContainerAllState.board.foreach{b =>
      val board = new Board(config,b)
      boardMap.put(b.boardId,board)
    }

    gameContainerAllState.ball.foreach{b =>
      val ball = new Ball(config,b)
      ballMap.put(ball.bId,ball)
    }

    gameContainerAllState.bricks.foreach{b =>
      val brick = new Brick(config,b)
      brickMap.put(b.bId,brick)
    }

    gameContainerAllState.boardMoveAction.foreach { bm =>
      val set = boardMoveAction.getOrElse(bm._1, mutable.HashSet[Byte]())
      bm._2.foreach(l => l.foreach(set.add))
      boardMoveAction.put(bm._1, set)
    }
  }

  protected def handleGameContainerState(gameContainerState: GameContainerState) = {
    val curFrame = systemFrame
    val startTime = System.currentTimeMillis()
    (curFrame until gameContainerState.f).foreach { _ =>
      super.update()
      if (esRecoverSupport) addGameSnapShot(systemFrame, getGameContainerAllState())
    }
    val endTime = System.currentTimeMillis()
    if (curFrame < gameContainerState.f) {
      println(s"handleGameContainerState update to now use Time=${endTime - startTime} and systemFrame=${systemFrame} sysFrame=${gameContainerState.f}")
    }

    if(!judge(gameContainerState)||systemFrame!=gameContainerState.f){
      systemFrame = gameContainerState.f
      boardMap.clear()
      boardMoveAction.clear()
    }
  }

  private def judge(gameContainerState: GameContainerState) = {
    gameContainerState.board match{
      case Some(boards) =>
        boards.forall { boardState =>
          boardMap.get(boardState.boardId) match {
            case Some(t) =>
              if (t.getBoardState() != boardState) {
                println(s"judge failed,because board=${boardState.boardId} no same,tankMap=${t.getBoardState()},gameContainer=$boardState")
                false
              } else true
            case None => {
              println(s"judge failed,because tank=${boardState.boardId} not exists....")
              true
            }
          }
        }
      case None =>
        println(s"game container client judge function no tanks---")
        true
    }
  }

  def receiveGameContainerAllState(gameContainerAllState: GameContainerAllState) = {
    gameContainerAllStateOpt = Some(gameContainerAllState)
 }

  def receiveGameContainerState(gameContainerState: GameContainerState) = {
    if (gameContainerState.f > systemFrame) {
      gameContainerState.board match {
        case Some(board) =>
          gameContainerStateOpt = Some(gameContainerState)
        case None =>
          gameContainerStateOpt match{
            case Some(state) =>
              gameContainerStateOpt = Some(NBGameEvent.GameContainerState(gameContainerState.f,state.board,state.boardMoveAction))
            case None =>
          }
      }
    } else if (gameContainerState.f == systemFrame) {
      gameContainerState.board match{
        case Some(board) =>
          info(s"收到同步数据，立即同步，curSystemFrame=${systemFrame},sync game container state frame=${gameContainerState.f}")
          gameContainerStateOpt = None
          handleGameContainerState(gameContainerState)
        case None =>
          info(s"收到同步帧号的数据")
      }
    } else {
      info(s"收到同步数据，但未同步，curSystemFrame=${systemFrame},sync game container state frame=${gameContainerState.f}")
    }
  }


  override def update(): Unit = {
    if (gameContainerAllStateOpt.nonEmpty) {
      val gameContainerAllState = gameContainerAllStateOpt.get
      info(s"立即同步所有数据，curSystemFrame=${systemFrame},sync game container state frame=${gameContainerAllState.f}")
      handleGameContainerAllState(gameContainerAllState)
      gameContainerAllStateOpt = None
      if (esRecoverSupport) {
        clearEsRecoverData()
        addGameSnapShot(systemFrame, this.getGameContainerAllState())
      }
    } else if (gameContainerStateOpt.nonEmpty && (gameContainerStateOpt.get.f - 1 == systemFrame || gameContainerStateOpt.get.f - 2 > systemFrame)) {
      info(s"同步数据，curSystemFrame=${systemFrame},sync game container state frame=${gameContainerStateOpt.get.f}")
      handleGameContainerState(gameContainerStateOpt.get)
      gameContainerStateOpt = None
      if (esRecoverSupport) {
        clearEsRecoverData()
        addGameSnapShot(systemFrame, this.getGameContainerAllState())
      }
    } else {
      super.update()
      if (esRecoverSupport) addGameSnapShot(systemFrame, getGameContainerAllState())
    }
  }

  override protected def clearEventWhenUpdate(): Unit = {
    if (esRecoverSupport) {
      addEventHistory(systemFrame, gameEventMap.getOrElse(systemFrame, Nil), actionEventMap.getOrElse(systemFrame, Nil))
    }
    gameEventMap -= systemFrame
    actionEventMap -= systemFrame
    systemFrame += 1
  }

  protected def rollbackUpdate(): Unit = {
    super.update()
    if (esRecoverSupport) addGameSnapShot(systemFrame, getGameContainerAllState())
  }

  def updateRank = {
    boardMap.map{board =>
      val bricks = brickMap.values.filter(brick => brick.boardId == board._1)
      Score(board._2.userId,board._2.name,board._2.boardId,bricks.size.toByte)
    }.toList
  }

  override def handleGameStart = {
    super.handleGameStart
  }

  def drawGame(time: Long, networkLatency: Long, dataSizeList: List[String]): Unit = {
    val offsetTime = math.min(time, config.frameDuration)
    if (!waitSyncData) {
      ctx.setLineCap("round")
      ctx.setLineJoin("round")
      boardMap.get(boardId) match {
        case Some(board) =>
          drawBackground()
          drawBricks()
          drawBoard(board, offsetTime)
          ballMap.get(ballId).foreach(b => drawBall(b,offsetTime))
          drawRank(updateRank)
          renderFps(networkLatency, dataSizeList)
          drawRoomNumber()
          if (board.canvasFrame >= 1) {
            board.canvasFrame += 1
          }
        case None =>
          println(s"there is no board $boardId")

      }
    }
  }

}
