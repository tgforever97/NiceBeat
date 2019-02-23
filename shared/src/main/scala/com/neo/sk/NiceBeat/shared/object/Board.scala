package com.neo.sk.NiceBeat.shared.`object`

import java.awt.event.KeyEvent

import com.neo.sk.NiceBeat.shared.config.NiceBeatConfig
import com.neo.sk.NiceBeat.shared.model
import com.neo.sk.NiceBeat.shared.model.Constants.{BoardColor, DirectionType}
import com.neo.sk.NiceBeat.shared.model.{Point, Rectangle}
import com.sun.org.apache.xpath.internal.functions.FuncFalse
import sun.print.BackgroundLookupListener


/**
  * Created by hongruying on 2018/8/22
  * Edited by wmy
  */

case class BoardState(userId:String,
                      name:String,
                      boardId:Int,
                      position:Point,
                      speed:Point,
                      direction:Float,
                      ballLevel:Byte,
                      boardColorType:Byte,
                      isMove:Boolean)

case class Board(
                   config: NiceBeatConfig,
                   userId: String,
                   name: String,
                   boardId: Int,
                   boardColorType: Byte,
                   ballLevel: Byte,
                   var speed: Point,
                   var direction: Float,
                   var position: Point,
                   var isMove: Boolean) extends RectangleObjectOfGame {


  def this(config: NiceBeatConfig, boardState: BoardState) {
    this(config, boardState.userId, boardState.name, boardState.boardId, boardState.boardColorType, boardState.ballLevel, boardState.speed, boardState.direction, boardState.position, boardState.isMove)
  }

  var isFakeMove = false
  var fakePosition = Point(0, 0)
  var canvasFrame = 0
  var fakeFrame = 0l
  val height: Float = config.getBoardHeight
  val width: Float = config.getBoardWidth
  val collisionOffset: Float = config.getBoardWO

  def getPosition4Animation(boundary: Point, offSetTime: Long): Point = {
    if(isMove){
      val targetSpeed = config.getBoardMoveDistanceByFrame.rotate(this.direction)
      modifyMove(boundary,this.position + targetSpeed / config.frameDuration * offSetTime)
    }
    else
      this.position
  }

  def getBoardColor() = {
    BoardColor.boardColorList(this.boardColorType)
  }

  def getBallLevel = ballLevel

  private def getBoardBallDamage()(implicit config: NiceBeatConfig): Int = {
    config.getBallDamage(ballLevel)
  }

  // 获取弹板状态
  def getBoardState(): BoardState = {
    BoardState(userId, name, boardId, position, speed, direction, ballLevel, boardColorType, isMove)
  }

  //判断是否到了地图边界
  def modifyMove(boundary: Point, original:Point) = {
    if (original.x + config.getBoardWidth/2 >= boundary.x)
      Point(boundary.x - config.getBoardWidth/2,original.y)
    else if(original.x - config.getBoardWidth/2 <= 0)
      Point(config.getBoardWidth/2,original.y)
    else
      original
  }

  // 根据方向移动
  def move(boundary: Point): Unit = {
    if(isMove) {
      val targetSpeed = config.getBoardMoveDistanceByFrame.rotate(this.direction)
      this.position = modifyMove(boundary,this.position + targetSpeed)
    } else
      this.position = this.position
  }

  /**
    * 根据弹板的按键修改弹板的方向状态
    **/
  final def setBoardDirection(actionSet: Set[Byte]) = {
    val targetDirectionOpt = getDirection(actionSet)
    if (targetDirectionOpt.nonEmpty) {
      this.direction = targetDirectionOpt.get
      isMove = true
    }
    else isMove = false
  }

  import scala.language.implicitConversions

  private final def getDirection(actionSet: Set[Byte]): Option[Float] = {
    implicit def changeInt2Byte(i: Int): Byte = i.toByte

    if (actionSet.contains(KeyEvent.VK_RIGHT)) {
      Some(DirectionType.right)
    } else if (actionSet.contains(KeyEvent.VK_LEFT)) {
      Some(DirectionType.left.toFloat)
    } else None
  }

}