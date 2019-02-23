package com.neo.sk.NiceBeat.front.tankClient.control

import java.util.concurrent.atomic.AtomicInteger

import com.neo.sk.NiceBeat.front.common.{Constants, Routes}
import com.neo.sk.NiceBeat.front.components.StartGameModal
import com.neo.sk.NiceBeat.front.utils.{JsFunc, Shortcut}
import com.neo.sk.NiceBeat.shared.`object`.Board
import com.neo.sk.NiceBeat.shared.game.GameContainerClientImpl
import com.neo.sk.NiceBeat.shared.model.Constants.GameState
import com.neo.sk.NiceBeat.shared.model.Point
import com.neo.sk.NiceBeat.shared.protocol.NBGameEvent
import org.scalajs.dom
import org.scalajs.dom.ext.{Color, KeyCode}
import org.scalajs.dom.raw.MouseEvent

import scala.collection.mutable
import scala.xml.Elem

/**
  * User: sky
  * Date: 2018/10/29
  * Time: 13:00
  *
  * edited by wmy
  */
class GamePlayHolderImpl(name: String) extends GameHolder(name) {
  private[this] val actionSerialNumGenerator = new AtomicInteger(0)
  private var spaceKeyUpState = true

  private var poKeyBoardFrame = 0L
  private val preExecuteFrameOffset = com.neo.sk.NiceBeat.shared.model.Constants.PreExecuteFrameOffset
  private val startGameModal = new StartGameModal(gameStateVar, start)

  private val watchKeys = Set(
    KeyCode.Left,
    KeyCode.Right,
  )

  private val myKeySet = mutable.HashSet[Int]()

  def getActionSerialNum: Byte = (actionSerialNumGenerator.getAndIncrement()%127).toByte

  def getStartGameModal(): Elem = {
    startGameModal.render
  }

  private def start(name: String): Unit = {
    canvas.getCanvas.focus()
    if (firstCome) {
      firstCome = false
      addUserActionListenEvent()
      setGameState(GameState.loadingPlay)
      webSocketClient.setup(Routes.getJoinGameWebSocketUri(name))
      gameLoop()

    } else if(webSocketClient.getWsState) {
      webSocketClient.sendMsg(NBGameEvent.RestartGame(name))
      setGameState(GameState.loadingPlay)
      gameLoop()
    }
    else {
      JsFunc.alert("网络连接失败，请重新刷新")
    }
  }

  private def addUserActionListenEvent(): Unit = {
    canvas.getCanvas.focus()
    canvas.getCanvas.onkeydown = { e: dom.KeyboardEvent =>
      if (gameContainerOpt.nonEmpty && gameState == GameState.play) {
        /**
          * 增加按键操作
          **/
        if (watchKeys.contains(e.keyCode) && !myKeySet.contains(e.keyCode)) {
          myKeySet.add(e.keyCode)
          val preExecuteAction = NBGameEvent.UserPressKeyDown(gameContainerOpt.get.myBoardId, gameContainerOpt.get.systemFrame + preExecuteFrameOffset, e.keyCode.toByte, getActionSerialNum)
          gameContainerOpt.get.preExecuteUserEvent(preExecuteAction)
          sendMsg2Server(preExecuteAction)
          if (com.neo.sk.NiceBeat.shared.model.Constants.fakeRender) {
            gameContainerOpt.get.addMyAction(preExecuteAction)
          }
          e.preventDefault()
        }
      }
    }
    canvas.getCanvas.onkeyup = { e: dom.KeyboardEvent =>
      if (gameContainerOpt.nonEmpty && gameState == GameState.play) {
        if (watchKeys.contains(e.keyCode)) {
          myKeySet.remove(e.keyCode)
          val preExecuteAction = NBGameEvent.UserPressKeyUp(gameContainerOpt.get.myBoardId, gameContainerOpt.get.systemFrame + preExecuteFrameOffset, e.keyCode.toByte, getActionSerialNum)
          gameContainerOpt.get.preExecuteUserEvent(preExecuteAction)
          sendMsg2Server(preExecuteAction)
          if (com.neo.sk.NiceBeat.shared.model.Constants.fakeRender) {
            gameContainerOpt.get.addMyAction(preExecuteAction)
          }
          e.preventDefault()
        }
      }
    }
  }


  override protected def wsMessageHandler(data: NBGameEvent.WsMsgServer): Unit = {
    data match {
      case e: NBGameEvent.WsSuccess =>
        webSocketClient.sendMsg(NBGameEvent.StartGame(e.userName))

      case e: NBGameEvent.YourInfo =>
        println(s"玩家信息${e}")
        timer = Shortcut.schedule(gameLoop, e.config.frameDuration / e.config.playRate)
        gameContainerOpt = Some(GameContainerClientImpl(drawFrame, ctx, e.config, e.userId, e.boardId, e.ballId,  e.userName, canvasBoundary, canvasUnit))

      case e: NBGameEvent.YouAreKilled =>

        /**
          * 死亡重玩
          **/
        gameContainerOpt.foreach(_.drawGameStop(s"Defeat"))
        setGameState(GameState.stop)

      case e:NBGameEvent.YouWin =>

        gameContainerOpt.foreach(_.drawGameStop(s"Victory"))
        setGameState(GameState.stop)

      case e: NBGameEvent.SyncGameState =>
        gameContainerOpt.foreach(_.receiveGameContainerState(e.state))

      case e: NBGameEvent.SyncGameAllState =>
        gameContainerOpt.foreach(_.receiveGameContainerAllState(e.gState))
        dom.window.cancelAnimationFrame(nextFrame)
        nextFrame = dom.window.requestAnimationFrame(gameRender())
        setGameState(GameState.play)

      case e: NBGameEvent.UserActionEvent =>
        gameContainerOpt.foreach(_.receiveUserEvent(e))

      case e: NBGameEvent.GameEvent =>
        gameContainerOpt.foreach(_.receiveGameEvent(e))

      case e: NBGameEvent.PingPackage =>
        receivePingPackage(e)

      case NBGameEvent.RebuildWebSocket =>
        gameContainerOpt.foreach(_.drawReplayMsg("存在异地登录。。"))
        closeHolder

      case _ => println(s"unknow msg={sss}")
    }
  }
}
