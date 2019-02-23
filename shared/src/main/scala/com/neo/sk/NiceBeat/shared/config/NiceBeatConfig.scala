package com.neo.sk.NiceBeat.shared.config

import com.neo.sk.NiceBeat.shared.model.Point

/**
  * Created by hongruying on 2018/8/28
  * Edited by wmy on 2019/02/13
  */
final case class GridBoundary(width:Int,height:Int){
  def getBoundary:Point = Point(width,height)
}

final case class BoardMoveSpeed(
                                speedWidth:Int
                              ){
  def getBoardSpeed = Point(speedWidth,0)
}

final case class BoardParameters(
                                 boardSpeed:BoardMoveSpeed,
                                 width:Int,
                                 height:Int,
                                 collisionWidthOffset:Int,
                                 initPosition:Point
                               ){
}

final case class BrickParameters(
                                  blood:Int,
                                  num:Int,
                                  width:Int,
                                  height:Int,
                                  collisionWidthOffset:Int,
                                  xList:List[Int],
                                  yList:List[Int]
                                )

final case class BallParameters(
                                 ballLevelParameters:List[(Float,Int)], //size,damage length 2
                                 ballSpeed:Int,
                                 ){

  def getBallRadius(l:Byte) = {
    ballLevelParameters(l-1)._1
  }

  def getBulletDamage(l:Byte) = {
    ballLevelParameters(l-1)._2
  }

  def getBallRadiusByDamage(d:Int):Float = {
    ballLevelParameters.find(_._2 == d).map(_._1).getOrElse(ballLevelParameters.head._1)
  }

  def getBallLevelByDamage(d:Int):Byte = {
    (ballLevelParameters.zipWithIndex.find(_._1._2 == d).map(_._2).getOrElse(0) + 1).toByte
  }
}

trait NiceBeatConfig{
  def frameDuration:Long
  def playRate:Int

  //弹球相关
  def getBallRadius(l:Byte):Float
  def getBallDamage(l:Byte):Int
  def getBallLevel(damage:Int):Byte
  def getBallMaxLevel:Byte
  def getBallSpeed:Point
  def getBallRadiusByDamage(d:Int):Float
  def getBallInitPosition:Point
  def getBallMoveDistanceByFrame = getBallSpeed * frameDuration / 1000

  //边界
  def boundary:Point

  //砖块
  def brickBlood:Int
  def brickNum:Int
  def brickHeight:Int
  def brickWidth:Int
  def brickWO:Int
  def brickInitPosListX:List[Int]
  def brickInitPosListY:List[Int]


  //弹板相关
  def getBoardSpeed:Point
  def getBoardWidth:Int
  def getBoardHeight:Int
  def getBoardInitPosition:Point
  def getBoardMoveDistanceByFrame = getBoardSpeed * frameDuration / 1000
  def getBoardWO:Int

  def getNiceBeatConfigImpl(): NiceBeatConfigImpl

}

case class NiceBeatConfigImpl(
                               gridBoundary: GridBoundary,
                               frameDuration:Long,
                               playRate:Int,
                               ballParameters: BallParameters,
                               brickParameters: BrickParameters,
                               boardParameters: BoardParameters
                             ) extends NiceBeatConfig{

  def getNiceBeatConfigImpl(): NiceBeatConfigImpl = this


  def getBallRadius(l:Byte) = ballParameters.getBallRadius(l)
  def getBallDamage(l:Byte) = ballParameters.getBulletDamage(l)
  def getBallLevel(damage:Int):Byte = ballParameters.getBallLevelByDamage(damage)
  def getBallMaxLevel:Byte = ballParameters.ballLevelParameters.size.toByte
  def getBallRadiusByDamage(d:Int):Float = ballParameters.getBallRadiusByDamage(d)
  def getBallSpeed = Point(ballParameters.ballSpeed,0)
  def getBallInitPosition: Point = Point(boardParameters.initPosition.x,boardParameters.initPosition.y-10)

  def boundary = gridBoundary.getBoundary

  def brickBlood: Int = brickParameters.blood
  def brickNum: Int = brickParameters.num
  def brickHeight: Int = brickParameters.height
  def brickWidth: Int = brickParameters.width
  def brickWO: Int = brickParameters.collisionWidthOffset
  def brickInitPosListX:List[Int] = brickParameters.xList
  def brickInitPosListY:List[Int] = brickParameters.yList

  def getBoardSpeed: Point = boardParameters.boardSpeed.getBoardSpeed
  def getBoardWidth: Int = boardParameters.width
  def getBoardHeight: Int = boardParameters.height
  def getBoardInitPosition: Point = boardParameters.initPosition
  def getBoardWO: Int = boardParameters.collisionWidthOffset

}
