package com.neo.sk.NiceBeat.front.components

import com.neo.sk.NiceBeat.front.common.{Component, Constants}
import com.neo.sk.NiceBeat.shared.model.Constants.GameState
import mhtml.Var
import org.scalajs.dom
import org.scalajs.dom.ext.KeyCode
import org.scalajs.dom.KeyboardEvent
import scala.xml.Elem

/**
  * Created by hongruying on 2018/7/9
  */
class StartGameModal(gameState:Var[Int],startGame:String => Unit) extends Component{

  private val title = gameState.map{
    case GameState.firstCome => "NiceBeat"
    case GameState.stop => "重新开始"
    case _ => ""
  }

  private val divStyle = gameState.map{
    case GameState.firstCome => "display:block;"
    case GameState.stop => "display:block;"
    case _ => "display:none;"
  }

  private val inputDivStyle = gameState.map{
    case GameState.firstCome => "display:block;"
    case _ => "display:none;"
  }

  private var inputName = ""

  private val inputElem = <input id ="TankGameNameInput" onkeydown ={e:KeyboardEvent => clickEnter(e)}></input>
  private val button = <button id="start_button" class ="btn btn-info" onclick ={() => clickEnter()}>进入游戏</button>

  def clickEnter(e:KeyboardEvent):Unit = {
    if(e.keyCode == KeyCode.Enter){
      val name = dom.document.getElementById("TankGameNameInput").asInstanceOf[dom.html.Input].value
      if(name.nonEmpty){
        inputName = name
        startGame(name)
      }
      e.preventDefault()
    }
    if(e.keyCode == KeyCode.Space){
      if(inputName != "") startGame(inputName)
    }
  }

  def clickEnter():Unit = {
    val name = dom.document.getElementById("TankGameNameInput").asInstanceOf[dom.html.Input].value
    if(name.nonEmpty){
      inputName = name
      startGame(name)
    }
  }

  override def render: Elem = {
    <div style={divStyle}>
      <div class ="input_mask" id="input_mask_id" tabindex="-1" onkeydown ={e:KeyboardEvent => clickEnter(e)}></div>
      <div class ="input_div" style={inputDivStyle}>
        <div class ="input_title">{title}</div>
        <div>
          <p class="input_inline"><span class="input_des">名字</span>{inputElem}</p>
        </div>
        <div class ="input_button">
          <span>{button}</span>
        </div>
      </div>
    </div>

  }


}
