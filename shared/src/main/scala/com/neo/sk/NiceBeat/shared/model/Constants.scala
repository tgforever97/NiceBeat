package com.neo.sk.NiceBeat.shared.model

import com.neo.sk.NiceBeat.shared.protocol.NBGameEvent

import scala.util.Random

/**
  * Created by hongruying on 2018/8/28
  */
object Constants {

  val drawHistory = false

  object DirectionType {
    final val right:Float = 0
    final val left = math.Pi
  }

  object BoardColor{
    val boardColorList = List("#FFE4B5","#FFFACD","#97FFFF")
    def getRandomColorType(random:Random):Byte = random.nextInt(boardColorList.size).toByte
  }

  object BrickColor{
    val brickColorList = List("#FFF0F5","#FFE4E1","#FFEFD5","#F0FFFF")
    def getRandomColorType(random:Random):Byte = random.nextInt(brickColorList.size).toByte
  }

  val MaxBrickCount:Byte = 16

  object BallDirection{
    val directionListUp = List(45,60,75,90,105,120,135)
    val directionListLeft = List(135,150,165,180,195,210,225)
    def getRandomDirection(random: Random) = random.nextInt(directionListUp.size).toByte
  }

  object GameAnimation{
    val ballHitAnimationFrame = 8
    val brickDestroyAnimationFrame = 12
  }

  val PreExecuteFrameOffset = 2 //预执行2帧
  val fakeRender = false

  object GameState{
    val firstCome = 1
    val play = 2
    val stop = 3
    val loadingPlay = 4
    val leave = 6
  }

  final val WindowView = Point(192,108)

}
