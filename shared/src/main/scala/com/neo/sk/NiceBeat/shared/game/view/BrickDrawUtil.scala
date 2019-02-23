package com.neo.sk.NiceBeat.shared.game.view

import com.neo.sk.NiceBeat.shared.game.GameContainerClientImpl
import com.neo.sk.NiceBeat.shared.model.Constants
import com.neo.sk.NiceBeat.shared.model.Constants.BrickColor
import com.neo.sk.NiceBeat.shared.model.Point
import com.neo.sk.NiceBeat.shared.util.canvas.MiddleContext

import scala.collection.mutable

/**
  * Created by hongruying on 2018/8/29
  */
trait BrickDrawUtil{ this:GameContainerClientImpl =>
//
//  private val brickCanvasCacheMap = mutable.HashMap[(Byte, Boolean), Any]()
//
//  def updateObstacleSize(canvasSize:Point)={
//    brickCanvasCacheMap.clear()
//  }
//
//  private def generateBrickCacheCanvas(width: Float, height: Float, color: String): Any = {
//    val cacheCanvas = drawFrame.createCanvas((width * canvasUnit).toInt, (height * canvasUnit).toInt)
//    val ctxCache = cacheCanvas.getCtx
//    drawBrick(Point(width / 2, height / 2), width, height, 1, color, ctxCache)
//    cacheCanvas.change2Image()
//  }

  private def drawBrick(centerPosition:Point, width:Float, height:Float, bloodPercent:Float, color:String, context:MiddleContext = ctx):Unit = {
    context.setFill(color)
    context.beginPath()
    context.fillRec((centerPosition.x - width / 2) * canvasUnit, (centerPosition.y + height / 2 - bloodPercent * height) * canvasUnit,
      width * canvasUnit, bloodPercent * height * canvasUnit)
    context.closePath()


    context.setStrokeStyle("#E6E6FA")
    context.setLineWidth(1.2 * canvasUnit)
    context.beginPath()
    context.rect((centerPosition.x - width / 2) * canvasUnit, (centerPosition.y - height / 2) * canvasUnit,
      width * canvasUnit, height * canvasUnit)
    context.stroke()
    context.closePath()
    context.setLineWidth(1)
  }


  protected def drawBricks() = {
    brickMap.filter(b => b._2.boardId == boardId).values.foreach{ brick =>
      val position = brick.getBrickState().position
      drawBrick(position,brick.width,brick.height,brick.bloodPercent,BrickColor.brickColorList(brick.colorType),ctx)
    }
  }
}
