package CoSlide

import cats.Functor
import cats.data.NonEmptyList

final case class PaddingInfo(up : Double,
                             down : Double,
                             left : Double,
                             right : Double,
                            )

sealed abstract class PageF[+P]
object PageF {
  final case object Empty extends PageF[Nothing]
  final case class  Image(src : String) extends PageF[Nothing]

  final case class  Horizontal[+P](style : CoSlide.style.Horizontal,
                                   elements : NonEmptyList[P]
                                  ) extends PageF[P]
  final case class  Vertical[+P](style : CoSlide.style.Vertical,
                                 elements : NonEmptyList[P]
                                ) extends PageF[P]
  final case class  Padding[+P](info : PaddingInfo,
                                content : P
                               ) extends PageF[P]

  implicit val functorPage = new Functor[PageF] {
    def map[A, B](fa: PageF[A])(f: A => B) : PageF[B] = fa match {
      case Empty => Empty
      case Image(s) => Image(s)
      case Horizontal(s, e) => Horizontal(s, e.map(f))
      case Vertical(s,e) => Vertical(s, e.map(f))
      case Padding(i, c) => Padding(i, f(c))
    }
  }
}

trait Page[P] {
  def empty : P
  def image(src : String) : P

  def horizontal(style : CoSlide.style.Horizontal, elements : NonEmptyList[P]) : P
  def vertical(style : CoSlide.style.Vertical, elements : NonEmptyList[P]) : P
  def padding(info : PaddingInfo, content : P) : P

  final def algebra(pa : PageF[P]) : P = pa match {
    case PageF.Empty => empty
    case PageF.Image(s) => image(s)
    case PageF.Horizontal(s, e) => horizontal(s, e)
    case PageF.Vertical(s,e) => vertical(s, e)
    case PageF.Padding(i, c) => padding(i, c)
  }

  final val Algebra = new Algebra(algebra _)
}