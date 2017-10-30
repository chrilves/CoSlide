package CoSlide.style

import cats.Monoid

sealed abstract class TextIn
object TextIn {
  final object Normal extends TextIn
  final object Important extends TextIn
  final object Code extends TextIn
  final object Math extends TextIn
  final object Quote extends TextIn
}

final case class TextOut(color : Option[TextOut.Color] = None,
                         fontSize : Option[Int] = None,
                         bold : Option[Boolean] = None,
                         italic : Option[Boolean] = None,
                         code : Option[Boolean] = None
                        )
object TextOut {
  final case class Color(red : Int, green : Int, blue : Int)

  def color(c : Color) = TextOut(color = Some(c))
  def color(r :Int, g :Int, b:Int) = TextOut(color = Some(Color(r,g,b)))
  def fontSize(i : Int) = TextOut(fontSize = Some(i))
  val bold = TextOut(bold = Some(true))
  val italic = TextOut(italic = Some(true))
  val code = TextOut(code = Some(true))

  val default = TextOut()

  implicit val monoidTextOut = new Monoid[TextOut] {
    val empty = TextOut()

    def combine(x: TextOut, y: TextOut)  : TextOut =
      TextOut(
        color = x.color.orElse(y.color),
        fontSize = x.fontSize.orElse(y.fontSize),
        bold = x.bold.orElse(y.bold),
        italic = x.italic.orElse(y.italic),
        code = x.code.orElse(y.code)
      )
  }
}

sealed abstract class Horizontal
object Horizontal {
  final case object Normal extends Horizontal
}

sealed abstract class Vertical
object Vertical {
  final case object Normal extends Vertical
}