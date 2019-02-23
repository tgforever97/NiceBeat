package com.neo.sk.NiceBeat.shared.game.view

import java.time.OffsetTime

import com.neo.sk.NiceBeat.shared.`object`.{Ball, Board}
import com.neo.sk.NiceBeat.shared.game.GameContainerClientImpl
import com.neo.sk.NiceBeat.shared.model.Constants
import com.neo.sk.NiceBeat.shared.model.Point
import com.neo.sk.NiceBeat.shared.util.canvas.MiddleContext

import scala.collection.mutable

/**
  * Created by hongruying on 2018/8/29
  */
trait BoardDrawUtil{ this:GameContainerClientImpl =>

  private var canvasBoundary:Point = canvasSize

  def updateBoardSize(canvasSize:Point)={
    canvasBoundary = canvasSize
  }


  protected def drawBoard(board:Board,offsetTime: Long):Unit = {
    val position = board.getPosition4Animation(boundary,offsetTime)
    ctx.setFill(board.getBoardColor())
    ctx.beginPath()
    ctx.fillRec((position.x-board.getWidth/2) * canvasUnit, (position.y-board.getHeight/2) * canvasUnit,board.getWidth * canvasUnit,board.getHeight *canvasUnit)
    ctx.closePath()
  }

  protected def drawBall(ball:Ball,offsetTime: Long):Unit = {
    val position = ball.getPosition4Animation(boundary,offsetTime)
    ctx.beginPath()
    ctx.setFill("#00FFFF")
    ctx.arc(position.x * canvasUnit, position.y * canvasUnit, ball.getRadius * canvasUnit,0,360)
    ctx.fill()
    ctx.closePath()

  }

}
