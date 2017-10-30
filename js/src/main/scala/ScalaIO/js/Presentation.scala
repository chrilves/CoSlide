package ScalaIO.js

import java.net.URL

import CoSlide.JS.{TextHTMLElement, Web}
import CoSlide.style.TextOut
import org.scalajs.dom
import cats.syntax.monoid._
import CoSlide.style.TextOut._

import scalatags.JsDom.all.span
import cats.syntax.monoid._
import CoSlide.JS._

object Presentation {

  def main(args: Array[String]): Unit = {
    println("Je suis sur la page de controle!")
    val node = TextHTMLElement.text(color(127,127,127) |+| bold |+| fontSize(14) |+| code,
      """
        |trait Toto extends Any {
        |  def ppp(x : String) : Unit = prinln(x)
        |}
        |""".stripMargin) |+| TextHTMLElement.link(bold, "http://linuxfr.org", TextHTMLElement.text(default, "toto"))
    dom.window.document.body.appendChild(span(node : _*).render)
  }
}


/*

    val newWindow = dom.window.open(s"about:blank")

    newWindow.document.write(
      """
        |<html>
        |<head>
        |    <meta charset="UTF-8">
        |    <title>Les Slides</title>
        |</head>
        |<body>
        |</body>
        |</html>
      """.stripMargin
    )
    newWindow.document.close()
 */