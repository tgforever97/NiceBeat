package com.neo.sk.NiceBeat.front.utils.canvas

import com.neo.sk.NiceBeat.front.common.Routes
import com.neo.sk.NiceBeat.shared.util.canvas.MiddleImage
import org.scalajs.dom.html.Image
import org.scalajs.dom
import org.scalajs.dom.html

/**
  * Created by sky
  * Date on 2018/11/16
  * Time at 下午4:51
  */
object MiddleImageInJs {
  def apply(url: String): MiddleImageInJs = new MiddleImageInJs(url)
}

class MiddleImageInJs extends MiddleImage {
  private[this] var image: Image = _

  def this(url: String) = {
    this()
    image = dom.document.createElement("img").asInstanceOf[html.Image]
    image.setAttribute("src", Routes.base + "/static" + url)
  }

  def getImage = image

  override def isComplete: Boolean = image.complete

}
