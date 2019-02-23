package com.neo.sk.NiceBeat.shared.game.view

import com.neo.sk.NiceBeat.shared.`object`.Board
import com.neo.sk.NiceBeat.shared.game.GameContainerClientImpl
import com.neo.sk.NiceBeat.shared.util.canvas.{MiddleCanvas, MiddleContext, MiddleFrame, MiddleImage}
import com.neo.sk.NiceBeat.shared.model.Constants
import com.neo.sk.NiceBeat.shared.model.{Constants, Point, Score}
import scala.collection.mutable

/**
  * Created by hongruying on 2018/8/29
  */
trait BackgroundDrawUtil{ this:GameContainerClientImpl =>

  private val cacheCanvasMap = mutable.HashMap.empty[String, Any]
  private var canvasBoundary:Point = canvasSize
  private val rankWidth = 42
  private val rankHeight = 30
  private val rankCanvas = drawFrame.createCanvas(math.max(rankWidth * canvasUnit, 42 * 10),math.max(rankHeight * canvasUnit, 30 * 10))


  def updateBackSize(canvasSize:Point)={
    cacheCanvasMap.clear()
    canvasBoundary = canvasSize
  }

  private def clearScreen(color:String, alpha:Double, width:Float = canvasBoundary.x, height:Float = canvasBoundary.y, middleCanvas:MiddleContext , start:Point = Point(0,0)):Unit = {
    middleCanvas.setFill(color)
    middleCanvas.setGlobalAlpha(alpha)
    middleCanvas.fillRec(start.x * canvasUnit, start.y * canvasUnit,  width * this.canvasUnit, height * this.canvasUnit)
    middleCanvas.setGlobalAlpha(1)
  }

  def drawProgressBar(curCount:Byte,maxCount:Byte,start:Point,length:Float,color:String,info:String,context:MiddleContext) = {
    //进度条背景色
    context.setStrokeStyle("#E6E6FA")
    context.setLineCap("round")
    context.setLineWidth(6 * canvasUnit)
    context.beginPath()
    context.moveTo(start.x * canvasUnit,start.y * canvasUnit)
    context.lineTo((start.x+length) * canvasUnit,start.y * canvasUnit)
    context.stroke()
    context.closePath()

    //进度条白色填充
    context.setStrokeStyle("#BEBEBE")
    context.setLineCap("round")
    context.setLineWidth(4.4 * canvasUnit)
    context.beginPath()
    context.moveTo(start.x * canvasUnit,start.y * canvasUnit)
    context.lineTo((start.x+length) * canvasUnit,start.y * canvasUnit)
    context.stroke()
    context.closePath()

    //进度条根据砖块数量填充
    context.setLineWidth(4.4 * canvasUnit)
    context.setStrokeStyle(color)
    context.setLineCap("round")
    context.beginPath()
    context.moveTo(start.x * canvasUnit,start.y * canvasUnit)
    context.lineTo((start.x + curCount * (length / maxCount)) * canvasUnit,start.y * canvasUnit)
    context.stroke()
    context.closePath()

    //数字显示
    context.setFont("Arial", "normal", 4 * canvasUnit)
    context.setTextAlign("center")
    context.setTextBaseline("middle")
    context.setFill("#696969")
    context.fillText(s"$curCount", (start.x + curCount * (length / maxCount) + 6) * canvasUnit, start.y * canvasUnit)
    context.fillText(info,(start.x + length + 14) * canvasUnit,start.y * canvasUnit)
  }

  protected def generateRankCanvas(rank:List[Score]) = {
    val rankCtx = rankCanvas.getCtx
    var idx = 0
    rank.foreach{r =>
      drawProgressBar(r.brickCount,Constants.MaxBrickCount,Point(3,5 + idx),40,"#FFE4E1",r.userName.take(3),rankCtx)
      idx += 10
    }
    rankCanvas.change2Image()
  }

  protected def drawBackground():Unit = {
    clearScreen("#BEBEBE",1,canvasBoundary.x,canvasBoundary.y,ctx)
    clearScreen("#E8E8E8",1,boundary.x+0.5f,canvasBoundary.y,ctx)
  }

  protected def drawRank(rank:List[Score]):Unit = {
//    ctx.drawImage(generateRankCanvas(currentRank),(boundary.x + 4) * canvasUnit,0)
    var idx = 0
    rank.foreach{r =>
      drawProgressBar(r.brickCount,Constants.MaxBrickCount,Point(7+boundary.x,5 + idx),40,"#FFE4E1",r.userName.take(3),ctx)
      idx += 10
    }
  }

  protected def drawRoomNumber():Unit = {
    ctx.beginPath()
    ctx.setStrokeStyle("rgb(0,0,0)")
    ctx.setTextAlign("left")
    ctx.setFont("Arial","normal",3*canvasUnit)
    ctx.setLineWidth(1)
    ctx.strokeText(s"当前在线人数： ${boardMap.size}", 0,(canvasBoundary.y - 6) * canvasUnit , 20 * canvasUnit)

    ctx.beginPath()
    ctx.setFont("Helvetica", "normal",2 * canvasUnit)
    //      ctx.setTextAlign(TextAlignment.JUSTIFY)
    ctx.setFill("rgb(0,0,0)")
  }


}
