package com.neo.sk.NiceBeat.shared.`object`

import com.neo.sk.NiceBeat.shared.config.NiceBeatConfig
import com.neo.sk.NiceBeat.shared.model
import com.neo.sk.NiceBeat.shared.model.Constants.BallDirection
import com.neo.sk.NiceBeat.shared.model.Point

import scala.util.Random

/**
  * Created by hongruying on 2018/7/8
  * Edited by wmy
  * 弹球
  */

case class BallState(bId:Int, boardId:Int, direction:Float, speed:Point, position:Point, damage:Byte, isMove:Boolean)


case class Ball(
                 config:NiceBeatConfig,
                 bId:Int,
                 boardId:Int,
                 var direction:Float,
                 var speed:Point,
                 var position:Point,
                 var damage:Int,
                 var isMove:Boolean//威力
                 ) extends CircleObjectOfGame{

  def this(config:NiceBeatConfig, ballState: BallState){
    this(config,ballState.bId,ballState.boardId,ballState.direction,ballState.speed,ballState.position,ballState.damage.toInt,ballState.isMove)
  }

  override val radius: Float = config.getBallRadiusByDamage(damage)
  private var targetPosition = Point(0,0)

  // 获取弹球外形
  override def getObjectRect(): model.Rectangle = {
    model.Rectangle(this.position - Point(this.radius,this.radius),this.position + Point(this.radius, this.radius))
  }


  def getBallState(): BallState = {
    BallState(bId,boardId,direction,speed,position,damage.toByte,isMove)
  }

  def changeMoveTrue = {
    this.isMove = true
  }

  //弹球碰撞检测
  def isIntersectsObject(o: ObjectOfGame):Boolean = {
    this.isIntersects(o)
  }

  // 弹球落地判断
  def isDead(boundary:Point):Boolean = {
    if(this.position.y >= boundary.y - config.getBoardHeight/2)
      true
    else
      false
  }

  def move(boundary:Point,ballDeadCallBack:Ball => Unit) = {
    if(isDead(boundary) && isMove){
      ballDeadCallBack(this)
      isMove = false
    }
    else if(isMove){
      this.position = modifyBoundary(boundary,this.position + config.getBallMoveDistanceByFrame.rotate(this.direction))
    }else
      this.position = this.position
  }

  def modifyBoundary(boundary:Point,original:Point):Point = {
    var tempPoint = original
    if(original.x + radius >= boundary.x)
      tempPoint = Point(boundary.x-radius,tempPoint.y)
    else if(original.x - radius <= 0)
      tempPoint = Point(radius,tempPoint.y)
    if(original.y + radius >= boundary.y)
      tempPoint = Point(tempPoint.x,boundary.y-radius)
    else if(original.y - radius <= 0)
      tempPoint = Point(tempPoint.x,radius)
    tempPoint
  }


  // 检测是否弹球有碰撞到，碰撞到，执行回调函数
  def checkCollisionObject[T <: ObjectOfGame](o:T,collisionCallBack:T => Unit):Unit = {
    if(this.isIntersects(o)){
      println(s"collision happen")
      collisionCallBack(o)
    }
  }

  def checkCollisionWall(boundary:Point,collisionCallBack:Unit) = {
    if(this.position.y == radius + config.getBoardWO/2 || this.position.x == radius + config.getBoardWO/2 || this.position.x + radius + config.getBoardWO/2 == boundary.x)
      collisionCallBack
  }


  def getPosition4Animation(boundary:Point,offsetTime:Long) = {
    if(isMove){
      val targetSpeed = config.getBallMoveDistanceByFrame.rotate(this.direction)
      val moveAfterModify = modifyBoundary(boundary,this.position + targetSpeed / config.frameDuration * offsetTime)
      println(s"time $offsetTime == ${targetSpeed / config.frameDuration * offsetTime}")
      moveAfterModify
    }
    else
      this.position

  }

  def getBallLevel() = {
    config.getBallLevel(damage)
  }

  def setBallDirection(direction:Float) = {
    this.direction = direction
  }

  def setBallPosition(a:Point,b:Point,width:Float,height:Float) = {
    var temp = b
    if(a.y >= b.y)
      temp = Point(temp.x,a.y-radius-height/2)
    else
      temp = Point(temp.x,a.y+radius+height/2)

    this.position = temp
  }
}

