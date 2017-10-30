package ScalaIO.js

import java.net.URL

import CoSlide.JS.{TextHTML, Utils}
import CoSlide.style.TextOut
import org.scalajs.dom
import cats.syntax.monoid._
import CoSlide.style.TextOut._

import scalatags.JsDom.all.span
import cats.syntax.monoid._
import CoSlide.JS._

object Presentation {

  import TextSyntax._

  def main(args: Array[String]): Unit = {
    println("Je suis sur la page de controle!")
    val node = text(
      """
        |trait Toto extends Any {
        |  def ppp(x : String) : Unit = prinln(x)
        |}
        |""".stripMargin, color(127,127,127) |+| bold |+| fontSize(2) |+| code) |+|
      link("http://linuxfr.org", text("toto"), bold)
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