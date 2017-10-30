package CoSlide

import cats.Monoid
import cats.data.NonEmptyList
import cats.functor.Bifunctor

/////////////////////////////////////////////////
//  Texte
//

sealed abstract class TextF[+S, +T]
object TextF {
  final case class  Text[+S](style : S, text : String) extends TextF[S, Nothing]
  final case class  Link[+S, +T](style : S, url : String, children : T) extends TextF[S,  T]

  implicit val textFunctor = new Bifunctor[TextF] {
    def bimap[A, B, C, D](fab: TextF[A, B])(f: A => C, g: B => D) : TextF[C,D] = fab match {
      case Text(s,t) => Text(f(s),t)
      case Link(s, u, t) => Link(f(s), u, g(t))
    }
  }
}

trait Text[-S, T] extends Monoid[T] {
  def text(style : S, text : String) : T
  def link(style : S, url : String, children : T) : T

  final def algebra(ta : VTextF[S,T]) : T = {
    def aux(t : TextF[S, T]) : T = t match {
      case TextF.Text(s,t) => text(s,t)
      case TextF.Link(s,u,t) => link(s, u, t)
    }
    ta.foldLeft(empty) { case (acc, u) => combine(acc, aux(u)) }
  }

  final def Algebra[S2 <: S] = new Algebra[VTextF[S2,?], T](algebra _)
}

/////////////////////////////////////////////////
//  Slide
//

sealed abstract class SlideF[+T, +S]
object SlideF {
  final case class Image(src : String) extends SlideF[Nothing, Nothing]
  final case class Note(title : String, comment : String) extends SlideF[Nothing, Nothing]
  final case class Bullets[+S](bullets : NonEmptyList[S]) extends SlideF[Nothing, S]
  final case class Section[+T,+S](title : T, content : S) extends SlideF[T, S]
  final case class Paragraph[+S](content : NonEmptyList[S]) extends SlideF[Nothing, S]

  implicit def slideFunctor = new Bifunctor[SlideF] {
    def bimap[A, B, C, D](fab: SlideF[A, B])(f: A => C, g: B => D) : SlideF[C,D] = fab match {
      case Image(s) => Image(s)
      case Note(t,s) => Note(t,s)
      case Bullets(b) => Bullets(b.map(g))
      case Section(t,c) => Section(f(t), g(c))
      case Paragraph(c) => Paragraph(c.map(g))
    }
  }
}

trait Slide[-T, P] extends Monoid[P] {
  def image(src : String) : P
  def note(title : String, comment : String) : P

  def bullets(bullets : NonEmptyList[P]) : P
  def section(title : T, content : P) : P
  def paragraph(a : NonEmptyList[P]) : P

  final def algebra(sa : SlideF[T,P]) : P = sa match {
    case SlideF.Image(s) => image(s)
    case SlideF.Note(t,s) => note(t,s)
    case SlideF.Bullets(b) => bullets(b)
    case SlideF.Section(t,c) => section(t, c)
    case SlideF.Paragraph(c) => paragraph(c)
  }

  final def Algebra[T2 <: T] = new Algebra[SlideF[T2,?], P](algebra _)
}


////////////////////////////////////////////////////////////////////////////
// Table Of Contents
//

final case class  TableOfContentsF[+T, +R](title : T, subSections : R) {
  def bimap[U, S](f : T => U, g : R => S) : TableOfContentsF[U, S] =
    TableOfContentsF[U,S](f(title), g(subSections))
}
