package com.neo.sk.NiceBeat.front.tankClient

import com.neo.sk.NiceBeat.shared.protocol.NBGameEvent
import com.neo.sk.NiceBeat.shared.protocol.NBGameEvent.SyncGameAllState
import org.scalajs.dom.Blob
import org.scalajs.dom.raw._
import org.seekloud.byteobject.ByteObject.bytesDecode
import org.seekloud.byteobject.MiddleBufferInJs

import scala.scalajs.js.typedarray.ArrayBuffer


/**
  * Created by hongruying on 2018/7/9
  */
case class WebSocketClient(
                       connectSuccessCallback: Event => Unit,
                       connectErrorCallback:Event => Unit,
                       messageHandler:NBGameEvent.WsMsgServer => Unit,
                       closeCallback:Event => Unit,
                       setDateSize: (String,Double) => Unit
                     ) {



  private var wsSetup = false

  private var replay:Boolean = false

  private var webSocketStreamOpt : Option[WebSocket] = None

  def getWsState = wsSetup

  def setWsReplay(r:Boolean)={replay=r}

  private val sendBuffer:MiddleBufferInJs = new MiddleBufferInJs(4096)

  def sendMsg(msg:NBGameEvent.WsMsgFront) = {
    import org.seekloud.byteobject.ByteObject._
    webSocketStreamOpt.foreach{ s =>
      s.send(msg.fillMiddleBuffer(sendBuffer).result())
    }
  }


  def setup(wsUrl:String):Unit = {
    if(wsSetup){
      println(s"websocket已经启动")
    }else{
      val websocketStream = new WebSocket(wsUrl)

      webSocketStreamOpt = Some(websocketStream)
      websocketStream.onopen = { event: Event =>
        wsSetup = true
        connectSuccessCallback(event)
      }
      websocketStream.onerror = { event: Event =>
        wsSetup = false
        webSocketStreamOpt = None
        connectErrorCallback(event)
      }

      websocketStream.onmessage = { event: MessageEvent =>
        //        println(s"recv msg:${event.data.toString}")
        event.data match {
          case blobMsg:Blob =>
            val fr = new FileReader()
            fr.readAsArrayBuffer(blobMsg)
            fr.onloadend = { _: Event =>
              val buf = fr.result.asInstanceOf[ArrayBuffer]
              messageHandler(wsByteDecode(buf,blobMsg.size))
            }
          case jsonStringMsg:String =>
            import io.circe.generic.auto._
            import io.circe.parser._
            val data = decode[NBGameEvent.WsMsgServer](jsonStringMsg).right.get
            messageHandler(data)
          case unknow =>  println(s"recv unknow msg:${unknow}")
        }

      }

      websocketStream.onclose = { event: Event =>
        wsSetup = false
        webSocketStreamOpt = None
        closeCallback(event)
      }
    }
  }

  def closeWs={
    wsSetup = false
    webSocketStreamOpt.foreach(_.close())
    webSocketStreamOpt = None
  }

  import org.seekloud.byteobject.ByteObject._

  private def wsByteDecode(a:ArrayBuffer,s:Double):NBGameEvent.WsMsgServer={
    val middleDataInJs = new MiddleBufferInJs(a)
    bytesDecode[NBGameEvent.WsMsgServer](middleDataInJs) match {
      case Right(r) =>
        try {
          setDateSize(s"${r.getClass.toString.split("NBGameEvent").last.drop(1)}",s)
        }catch {case exception: Exception=> println(exception.getCause)}
        r
      case Left(e) =>
        println(e.message)
        NBGameEvent.DecodeError()
    }
  }

}
