package com.neo.sk.NiceBeat.shared.`object`

import com.neo.sk.NiceBeat.shared.config.NiceBeatConfig
import com.neo.sk.NiceBeat.shared.model
import com.neo.sk.NiceBeat.shared.model.Constants.{BallDirection, CollisionType}
import com.neo.sk.NiceBeat.shared.model.{Point, Rectangle}

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
//  var collisionType = 0


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

  def move(boundary:Point,bricksList:List[Brick],board: Board,ballDeadCallBack:Ball => Unit) = {
    if(isDead(boundary) && isMove){
      ballDeadCallBack(this)
      isMove = false
    }
    else if(isMove){
        this.position = this.position + canMove(boundary,bricksList,board)
    }else
      this.position = this.position
  }

//  def modifyBoundary(boundary:Point,original:Point):Point = {
//    var tempPoint = original
//    if(original.x + radius >= boundary.x)
//      tempPoint = Point(boundary.x-radius,tempPoint.y)
//    else if(original.x - radius <= 0)
//      tempPoint = Point(radius,tempPoint.y)
//    if(original.y + radius >= boundary.y)
//      tempPoint = Point(tempPoint.x,boundary.y-radius)
//    else if(original.y - radius <= 0)
//      tempPoint = Point(tempPoint.x,radius)
//    tempPoint
//  }


  def canMove(boundary:Point,bricksList:List[Brick],board: Board) = {
    var targetMoveDis = config.getBallMoveDistanceByFrame.rotate(this.direction)
    val horizontalMove = targetMoveDis.copy(y = 0)
    val verticalMove = targetMoveDis.copy(x = 0)
    val tempPosition = this.position
    List(horizontalMove,verticalMove).foreach{d =>
      if(d.x != 0 || d.y != 0){
        val afterMovePOs = this.position + d
        this.position = afterMovePOs
        val moveRec = Rectangle(afterMovePOs - Point(radius, radius), afterMovePOs + Point(radius, radius))
        if(!bricksList.exists(b => b.isIntersects(this)) && !board.isIntersects(this) && moveRec.topLeft > model.Point(0, 0) && moveRec.downRight < boundary){
          this.position = tempPosition
        }else{
          targetMoveDis -= d
          this.position = tempPosition
        }
      }
    }
    println(s"targetmove $targetMoveDis")
    targetMoveDis
  }


//  def checkCollision[T <: ObjectOfGame](o:T,brickBallCallBack:T => Unit,boardBallCallBack:T => Unit,wallBallCallBack:Ball => Unit) = {
//    collisionType match{
//      case CollisionType.boardCollision =>
//        boardBallCallBack(o)
//      case CollisionType.brickCollision =>
//        brickBallCallBack(o)
//      case CollisionType.wallCollision =>
//        wallBallCallBack
//      case _ =>
//    }
//    collisionType = 0
//  }

  // 检测是否弹球有碰撞到，碰撞到，执行回调函数
  def checkCollisionObject[T <: ObjectOfGame](o:T,collisionCallBack:T => Unit):Unit = {
    if(this.isIntersects(o)){
      println(s"collision happen")
      collisionCallBack(o)
    }

  }

  def checkCollisionWall(boundary:Point,collisionCallBack:Unit) = {
    if(this.position.y <= radius + 2 || this.position.x <= radius + 2 || this.position.x + radius + 2 >= boundary.x)
      collisionCallBack
  }

  def checkCollisionBoard(board: Board,collisionCallBack:Board => Unit) = {
    val ballRec = Rectangle(this.position-Point(radius,radius)-Point(3,3),this.position+Point(radius,radius)+Point(3,3))
    val bRec = Rectangle(board.position-Point(board.width/2,board.height/2),board.position+Point(board.width/2,board.height/2))
    val (a,b,c,d) = (ballRec.topLeft.x,ballRec.topLeft.y,ballRec.downRight.x,ballRec.downRight.y)
    val (e,f,g,h) = (bRec.topLeft.x,bRec.topLeft.y,bRec.downRight.x,bRec.downRight.y)
    val minx = math.max(a,e)
    val miny = math.max(b,f)
    val maxx = math.min(c,g)
    val maxy = math.min(d,h)
    if(!((minx > maxx) || (miny > maxy))) collisionCallBack(board)
  }

  def checkCollisionBrick(brick: Brick,collisionCallBack:Brick => Unit) = {
    val ballRec = Rectangle(this.position-Point(radius,radius)-Point(3,3),this.position+Point(radius,radius)+Point(3,3))
    val bRec = Rectangle(brick.position-Point(brick.width/2,brick.height/2),brick.position+Point(brick.width/2,brick.height/2))
    val (a,b,c,d) = (ballRec.topLeft.x,ballRec.topLeft.y,ballRec.downRight.x,ballRec.downRight.y)
    val (e,f,g,h) = (bRec.topLeft.x,bRec.topLeft.y,bRec.downRight.x,bRec.downRight.y)
    val minx = math.max(a,e)
    val miny = math.max(b,f)
    val maxx = math.min(c,g)
    val maxy = math.min(d,h)
    if(!((minx > maxx) || (miny > maxy))) collisionCallBack(brick)
  }

  def getPosition4Animation(boundary:Point,bricksList:List[Brick],board: Board,offsetTime:Long) = {
    if(isMove){
      val moveAfterModify = this.position + canMove(boundary,bricksList:List[Brick],board: Board) / config.frameDuration * offsetTime
      println(s"time $offsetTime == ${moveAfterModify}")
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

}

