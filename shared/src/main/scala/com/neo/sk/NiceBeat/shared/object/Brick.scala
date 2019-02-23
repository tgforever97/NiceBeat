package com.neo.sk.NiceBeat.shared.`object`

import com.neo.sk.NiceBeat.shared.config.NiceBeatConfig
import com.neo.sk.NiceBeat.shared.model
import com.neo.sk.NiceBeat.shared.model.{Constants, Point}
import com.neo.sk.NiceBeat.shared.`object`.RectangleObjectOfGame
import com.neo.sk.NiceBeat.shared.model.Constants.BrickColor

/**
  * Created by hongruying on 2018/8/22
  * Edited by wmy on 2019/02/13
  * 砖头元素
  */

case class BrickState(bId:Int, boardId:Int, blood:Int, position:Point, colorType:Byte)
case class Brick(
                  config:NiceBeatConfig,
                  bId: Int,
                  boardId: Int,
                  var curBlood: Int,
                  var position: Point,
                  colorType:Byte
                ) extends RectangleObjectOfGame{

  def this(config: NiceBeatConfig, brickState: BrickState) {
    this(config, brickState.bId, brickState.boardId, brickState.blood, brickState.position, brickState.colorType)
  }

  val maxBlood:Int = config.brickBlood
  val height: Float = config.brickHeight
  val width: Float = config.brickWidth
  val collisionOffset: Float = config.brickWO

  def getBrickState():BrickState = BrickState(bId,boardId,curBlood,position,colorType)

  def attackedDamage(d: Int): Unit = {
    curBlood -= d
  }

  def isLived: Boolean = {
    if(curBlood > 0) true
    else false
  }

  def getBrickColor = {
    BrickColor.brickColorList(this.colorType)
  }

  def bloodPercent:Float = this.curBlood.toFloat / maxBlood

}
