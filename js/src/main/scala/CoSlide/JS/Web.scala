package CoSlide.JS

import java.net.URL

import CoSlide.JS.Web.renderTextOut
import CoSlide.Text
import CoSlide.style.TextOut
import org.scalajs.dom
import org.scalajs.dom.html.Paragraph
import org.scalajs.dom.raw.HTMLElement

import scalatags.JsDom
import scalatags.JsDom.all._
import scalatags.JsDom.styles
import scalatags.generic.StylePair

object TextHTMLElement extends Text[CoSlide.style.TextOut, Seq[Modifier]] {
  def text(to: CoSlide.style.TextOut, text: String): Seq[Modifier] = {
    val style = renderTextOut(to)
    if (to.code == Some(true))
      Vector(pre(style : _*)(text))
    else
      Vector(span(style : _*)(text))
    //(code(`class` := "scala")
  }

  def link(to: CoSlide.style.TextOut,
           url: String,
           children : Seq[Modifier]
          ) : Seq[Modifier] =
    Vector(a(href := url)(renderTextOut(to) : _*)(children : _*))

  def empty : Seq[Modifier] = Vector.empty

  def combine(x: Seq[Modifier],
              y: Seq[Modifier]
             ) : Seq[Modifier] = x ++ y
}

object Web {
  def renderTextOut(to : TextOut): Seq[StylePair[dom.Element, String]] = {
    val color_ = to.color.map(c => styles.color := s"rgb(${c.red},${c.green},${c.blue})")
    val fontSize_ = to.fontSize.map(f => styles.fontSize := f.pt)

    def ifSet[A](o: Option[Boolean])(a: A): Option[A] = o match {
      case Some(true) => Some(a)
      case _ => None
    }

    val bold_   = ifSet(to.bold)(styles.fontWeight := "bold")
    val italic_ = ifSet(to.italic)(styles.fontStyle := "italic")
    val code_   = ifSet(to.code)(styles.fontFamily := "\"Courier New\", Courier, monospace")

    List(color_, fontSize_, bold_, italic_, code_).flatten
  }
}