package com.neo.sk.NiceBeat.shared.game.view

import com.neo.sk.NiceBeat.shared.game.GameContainerClientImpl
import com.neo.sk.NiceBeat.shared.model.Constants
import com.neo.sk.NiceBeat.shared.model.Point

/**
  * Created by hongruying on 2018/8/29
  */
trait FpsComponentsDrawUtil{ this:GameContainerClientImpl =>
  private var lastRenderTime = System.currentTimeMillis()
  private var lastRenderTimes = 0
  private var renderTimes = 0
  private val isRenderFps:Boolean = true
  private var canvasBoundary:Point=canvasSize

  def updateFpsSize(canvasSize:Point)={
    canvasBoundary = canvasSize
  }


  private def addFps() ={
    val time = System.currentTimeMillis()
    renderTimes += 1
    if(time - lastRenderTime > 1000){
      lastRenderTime = time
      lastRenderTimes = renderTimes
      renderTimes = 0
    }
  }

  protected def renderFps(networkLatency: Long, dataSizeList:List[String]) = {
    addFps()
    if(isRenderFps){
      ctx.setFont("Helvetica", "normal",2 * canvasUnit)
//      ctx.setTextAlign(TextAlignment.JUSTIFY)
      ctx.setFill("rgb(0,0,0)")
      ctx.setTextAlign("left")
      val fpsString = s"fps : $lastRenderTimes,  ping : ${networkLatency}ms"
      ctx.fillText(fpsString,0,(canvasBoundary.y - 2) * canvasUnit)
      ctx.setTextAlign("right")
      var i=18
      dataSizeList.foreach{ r=>
        ctx.fillText(r,canvasBoundary.x*canvasUnit,(canvasBoundary.y - i) * canvasUnit)
        i+=2
      }
    }

  }

}