package com.neo.sk.NiceBeat.front.pages

import com.neo.sk.NiceBeat.front.common.Page
import com.neo.sk.NiceBeat.front.tankClient.control.GamePlayHolderImpl
import com.neo.sk.NiceBeat.front.utils.Shortcut
import mhtml.Var
import org.scalajs.dom
import org.scalajs.dom.ext.Color
import org.scalajs.dom.html.Canvas
import mhtml._

import scala.xml.Elem

/**
  * Created by hongruying on 2018/7/7
  */
object NiceBeatDemo extends Page{

  private val canvas = <canvas id="GameView" tabindex="1"></canvas>


  private val modal = Var(emptyHTML)

  def init() = {
    val gameHolder = new GamePlayHolderImpl("GameView")
    val startGameModal = gameHolder.getStartGameModal()
    modal := startGameModal

  }



  override def render: Elem ={
    Shortcut.scheduleOnce(() =>init(),0)
    <div>
      <div >{modal}</div>
      {canvas}
    </div>
  }



}
