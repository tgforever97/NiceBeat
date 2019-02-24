package com.neo.sk.NiceBeat.front.tankClient.control

import com.neo.sk.NiceBeat.front.tankClient.{NetworkInfo, WebSocketClient}
import com.neo.sk.NiceBeat.front.utils.canvas.MiddleFrameInJs
import com.neo.sk.NiceBeat.front.utils.{JsFunc, Shortcut}
import com.neo.sk.NiceBeat.shared.`object`.Board
import com.neo.sk.NiceBeat.shared.game.GameContainerClientImpl
import com.neo.sk.NiceBeat.shared.model.Constants.GameState
import com.neo.sk.NiceBeat.shared.model.{Constants, Point}
import com.neo.sk.NiceBeat.shared.protocol.NBGameEvent
import com.neo.sk.NiceBeat.front.tankClient.WebSocketClient
import com.neo.sk.NiceBeat.front.utils.canvas.MiddleFrameInJs
import mhtml.Var
import org.scalajs.dom
import org.scalajs.dom.html.{Audio, Div, Script}
import org.scalajs.dom.raw.{Event, TouchEvent, VisibilityState}

import scala.collection.mutable

/**
  * User: sky
  * Date: 2018/10/29
  * Time: 12:47
  * 需要构造参数，所以重构为抽象类
  *
  * edited by wmy
  */
abstract class GameHolder(name: String) extends NetworkInfo {
  val drawFrame = new MiddleFrameInJs
  protected var canvasWidth = dom.window.innerWidth.toFloat
  protected var canvasHeight = dom.window.innerHeight.toFloat
  protected val canvas = drawFrame.createCanvas(name, canvasWidth, canvasHeight)
  protected val ctx = canvas.getCtx
  protected var canvasUnit = getCanvasUnit(canvasWidth)
  protected var canvasBoundary = Point(canvasWidth, canvasHeight) / canvasUnit
  protected var firstCome = true
  protected val gameStateVar: Var[Int] = Var(GameState.firstCome)
  protected var gameState: Int = GameState.firstCome
  protected var gameContainerOpt: Option[GameContainerClientImpl] = None // 这里存储board,ball信息
  protected val webSocketClient: WebSocketClient = WebSocketClient(wsConnectSuccess, wsConnectError, wsMessageHandler, wsConnectClose, setDateSize )
  protected var timer: Int = 0
  /**
    * 倒计时，config
    **/
  protected var nextFrame = 0
  protected var logicFrameTime = System.currentTimeMillis()

  def closeHolder = {
    dom.window.cancelAnimationFrame(nextFrame)
    Shortcut.cancelSchedule(timer)
    webSocketClient.closeWs
  }

  protected def gameRender(): Double => Unit = { d =>
    import com.neo.sk.NiceBeat.front.common.Constants
    val curTime = System.currentTimeMillis()
    val offsetTime = curTime - logicFrameTime
    drawGame(offsetTime)
    gameState match {
      case GameState.stop => gameContainerOpt.foreach(_.drawGameStop("Defeat"))
      case GameState.win => gameContainerOpt.foreach(_.drawGameStop("Victory"))
      case GameState.loadingPlay => gameContainerOpt.foreach(_.drawGameLoading())
      case _  =>
    }
    nextFrame = dom.window.requestAnimationFrame(gameRender())
  }


  protected def setGameState(s: Int): Unit = {
    gameStateVar := s
    gameState = s
  }

  protected def sendMsg2Server(msg: NBGameEvent.WsMsgFront): Unit = {
    if (gameState == GameState.play)
      webSocketClient.sendMsg(msg)
  }

  protected def checkScreenSize = {
    val newWidth = dom.window.innerWidth.toFloat
    val newHeight = dom.window.innerHeight.toFloat
    if (newWidth != canvasWidth || newHeight != canvasHeight) {
      canvasWidth = newWidth
      canvasHeight = newHeight
      canvasUnit = getCanvasUnit(canvasWidth)
      canvasBoundary = Point(canvasWidth, canvasHeight) / canvasUnit
      println(s"update screen=$canvasUnit,=${(canvasWidth, canvasHeight)}")
      canvas.setWidth(canvasWidth.toInt)
      canvas.setHeight(canvasHeight.toInt)
      gameContainerOpt.foreach { r =>
        r.updateClientSize(canvasBoundary, canvasUnit)
      }
    }
  }

  protected def gameLoop(): Unit = {
    checkScreenSize
    gameState match {
      case GameState.loadingPlay =>
        println(s"等待同步数据")

      case GameState.play =>
        gameContainerOpt.foreach(_.update())
        logicFrameTime = System.currentTimeMillis()
        ping()

      case GameState.stop =>
        dom.document.getElementById("input_mask_id").asInstanceOf[dom.html.Div].focus()
        gameContainerOpt.foreach(_.drawGameStop("Defeat"))
        dom.window.cancelAnimationFrame(nextFrame)
        Shortcut.cancelSchedule(timer)
        ping()

      case GameState.win =>
        dom.document.getElementById("input_mask_id").asInstanceOf[dom.html.Div].focus()
        gameContainerOpt.foreach(_.drawGameStop("Victory"))
        dom.window.cancelAnimationFrame(nextFrame)
        Shortcut.cancelSchedule(timer)
        ping()

      case _ => println(s"state=$gameState failed")
    }
  }

  private def drawGame(offsetTime: Long,supportLiveLimit:Boolean = false) = {
    gameContainerOpt.foreach(_.drawGame(offsetTime, getNetworkLatency,dataSizeList))
  }

  protected def wsConnectSuccess(e: Event) = {
    println(s"连接服务器成功")
    e
  }

  protected def wsConnectError(e: Event) = {
    JsFunc.alert("网络连接失败，请重新刷新")
    e
  }

  protected def wsConnectClose(e: Event) = {
    JsFunc.alert("网络连接失败，请重新刷新")
    e
  }

  protected def wsMessageHandler(data: NBGameEvent.WsMsgServer)


  protected def getCanvasUnit(canvasWidth: Float): Int = (canvasWidth / Constants.WindowView.x).toInt
}
