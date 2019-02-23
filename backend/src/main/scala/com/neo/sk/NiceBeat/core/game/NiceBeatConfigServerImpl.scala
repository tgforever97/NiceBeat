package com.neo.sk.NiceBeat.core.game

import com.neo.sk.NiceBeat.shared.model.Point
import com.neo.sk.NiceBeat.shared.config._
import com.typesafe.config.Config
import akka.util.Helpers
import scala.concurrent.duration._

/**
  * Created by hongruying on 2018/8/21
  */
final case class NiceBeatConfigServerImpl(
                                           config:Config
                                         ) extends NiceBeatConfig {

  import collection.JavaConverters._
  import Helpers.Requiring
  import Helpers.ConfigOps

  private[this] val gridBoundaryWidth = config.getInt("niceBeat.gridBoundary.width")
    .requiring(_ > 100,"minimum supported grid boundary width is 100")
  private[this] val gridBoundaryHeight = config.getInt("niceBeat.gridBoundary.height")
    .requiring(_ > 50,"minimum supported grid boundary height is 50")
  private[this] val gridBoundary = GridBoundary(gridBoundaryWidth,gridBoundaryHeight)

  private[this] val gameFameDuration = config.getLong("niceBeat.frameDuration")
    .requiring(t => t >= 1l,"minimum game frame duration is 1 ms")
  private[this] val gamePlayRate = config.getInt("niceBeat.playRate")
    .requiring(t => t >= 1,"minimum game playRate duration is 1")

  private[this] val ballRadius = config.getDoubleList("niceBeat.ball.ballRadius")
    .requiring(_.size() >= 2,"ball radius size has 2 type").asScala.toList.map(_.toFloat)
  private[this] val ballDamage = config.getIntList("niceBeat.ball.ballDamage")
    .requiring(_.size() >= 2,"bullet damage size has 2 type").asScala.toList.map(_.toInt)
  private[this] val ballSpeedData = config.getInt("niceBeat.ball.ballSpeed")
    .requiring(_ > 0,"minimum bullet speed is 1")
  private val ballParameters = BallParameters(ballRadius.zip(ballDamage),ballSpeedData)

  private[this] val brickWidthData = config.getInt("niceBeat.brick.width")
    .requiring(_ > 0,"minimum supported obstacle width is 1")
  private[this] val brickHeightData = config.getInt("niceBeat.brick.height")
    .requiring(_ > 0,"minimum supported obstacle width is 1")
  private[this] val collisionWOffset = config.getInt("niceBeat.brick.collisionWidthOffset")
    .requiring(_ > 0,"minimum supported obstacle width is 1")
  private[this] val brickBloodData = config.getInt("niceBeat.brick.blood")
    .requiring(_ > 0,"minimum supported brick blood is 1")
  private[this] val brickNumData = config.getInt("niceBeat.brick.num")
    .requiring(_ >= 0,"minimum supported brick num is 0")
  private[this] val brickInitPosX = config.getIntList("niceBeat.brick.position_x")
    .requiring(_.size() >= 4," ize is 4").asScala.toList.map(_.toInt)
  private[this] val brickInitPosY = config.getIntList("niceBeat.brick.position_y")
    .requiring(_.size() >= 4," ize is 4").asScala.toList.map(_.toInt)
  private val brickParameters = BrickParameters(brickBloodData,brickNumData,brickWidthData,brickHeightData,collisionWOffset,brickInitPosX,brickInitPosY)


  private[this] val boardSpeed = config.getInt("niceBeat.board.boardSpeed")
    .requiring(_ >= 0,"minimum speed size is 0")
  private[this] val boardWOData = config.getInt("niceBeat.board.collisionWidthOffset")
    .requiring(_ > 0,"minimum supported width is 1")
  private[this] val boardWidthData = config.getInt("niceBeat.board.width")
    .requiring(_ > 0,"minimum supported width is 1")
  private[this] val boardHeightData = config.getInt("niceBeat.board.height")
    .requiring(_ > 0,"minimum supported height is 1")
  private[this] val boardInitPosX = config.getInt("niceBeat.board.initPosition_x")
    .requiring(_ > 0,"minimum x is 0")
  private[this] val boardInitPosY = config.getInt("niceBeat.board.initPosition_y")
    .requiring(_ > 0,"minimum y is 0")
  private val boardParameters = BoardParameters(BoardMoveSpeed(boardSpeed),boardWidthData,boardHeightData,boardWOData,Point(boardInitPosX,boardInitPosY))
  private val niceBeatConfig = NiceBeatConfigImpl(gridBoundary,gameFameDuration,gamePlayRate,ballParameters,brickParameters,boardParameters)


  def getNiceBeatConfig:NiceBeatConfigImpl = niceBeatConfig
  def frameDuration:Long = niceBeatConfig.frameDuration
  def playRate:Int = niceBeatConfig.playRate

  def getBallRadius(l:Byte):Float = niceBeatConfig.getBallRadius(l)
  def getBallDamage(l:Byte):Int = niceBeatConfig.getBallDamage(l)
  def getBallSpeed:Point = niceBeatConfig.getBallSpeed
  def getBulletRadiusByDamage(d:Int):Float = niceBeatConfig.getBallRadiusByDamage(d)
  def getBallLevel(damage: Int): Byte = niceBeatConfig.getBallLevel(damage)
  def getBallRadiusByDamage(d: Int): Float = niceBeatConfig.getBallRadiusByDamage(d)
  def getBallMaxLevel: Byte = niceBeatConfig.getBallMaxLevel
  def getBallInitPosition: Point = niceBeatConfig.getBallInitPosition

  def boundary:Point = niceBeatConfig.boundary

  def brickWidth:Int = niceBeatConfig.brickWidth
  def brickHeight: Int = niceBeatConfig.brickHeight
  def brickBlood:Int = niceBeatConfig.brickBlood
  def brickNum:Int = niceBeatConfig.brickNum
  def brickWO: Int = niceBeatConfig.brickWO
  def brickInitPosListX: List[Int] = niceBeatConfig.brickInitPosListX
  def brickInitPosListY: List[Int] = niceBeatConfig.brickInitPosListY

  def getBoardWidth:Int = niceBeatConfig.getBoardWidth
  def getBoardHeight:Int = niceBeatConfig.getBoardHeight
  def getBoardWO: Int = niceBeatConfig.getBoardWO
  def getBoardSpeed: Point = niceBeatConfig.getBoardSpeed
  def getBoardInitPosition: Point = niceBeatConfig.getBoardInitPosition

  def getNiceBeatConfigImpl(): NiceBeatConfigImpl = niceBeatConfig
}
