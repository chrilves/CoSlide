package CoSlide

import CoSlide.utils._
import cats.data.NonEmptyList
import cats.syntax.functor._
import cats.{Functor, Monoid}

////////////////////////////////////////////////////
// Placement

sealed abstract class Alignment
object Alignment {
  final case object Left   extends Alignment
  final case object Center extends Alignment
  final case object Right  extends Alignment
}

final case class PlacementInfo(up : Option[Double] = None,
                               down : Option[Double] = None,
                               left : Option[Double] = None,
                               right : Option[Double] = None,
                               horizontal : Option[Alignment] = None,
                               vertical : Option[Alignment] = None
                              )
object PlacementInfo {
  def up(x : Double) = PlacementInfo(up = Some(x))
  def down(x : Double) = PlacementInfo(down = Some(x))
  def left(x : Double) = PlacementInfo(left = Some(x))
  def right(x : Double) = PlacementInfo(right = Some(x))
  def horizontal(x : Alignment) = PlacementInfo(horizontal = Some(x))
  def vertical(x : Alignment) = PlacementInfo(vertical = Some(x))

  val default = PlacementInfo()

  implicit val placementInfoMonoid = new Monoid[PlacementInfo] {
    val empty: PlacementInfo = PlacementInfo()

    def combine(x: PlacementInfo, y: PlacementInfo): PlacementInfo =
      PlacementInfo(
        up = x.up.orElse(y.up),
        down = x.down.orElse(y.down),
        left = x.left.orElse(y.left),
        right = x.right.orElse(y.right),
        horizontal = x.horizontal.orElse(y.horizontal),
        vertical = x.vertical.orElse(y.vertical),
      )
  }
}

////////////////////////////////////////////////////
// Page

sealed abstract class PageF[+P]
object PageF {
  final case object Empty extends PageF[Nothing]

  final case class  Image(src : String) extends PageF[Nothing]
  final case class  Code(code : String) extends PageF[Nothing]

  final case class  Placement[+P](info : PlacementInfo, content : P) extends PageF[P]

  final case class  HorizontalSequence[+P](elements : NonEmptyList[P]) extends PageF[P]
  final case class  VerticalSequence[+P](s : style.VerticalSequence, elements : NonEmptyList[P]) extends PageF[P]

  final case class  HorizontalGrid[+P](elements : Partition[P]) extends PageF[P]
  final case class  VerticalGrid[+P](elements : Partition[P]) extends PageF[P]

  implicit val functorPage = new Functor[PageF] {
    def map[A, B](fa: PageF[A])(f: A => B) : PageF[B] = fa match {
      case Empty => Empty
      case Image(s) => Image(s)
      case Code(s) => Code(s)
      case HorizontalSequence(e) => HorizontalSequence(e.map(f))
      case VerticalSequence(s, e) => VerticalSequence(s, e.map(f))
      case HorizontalGrid(e) => HorizontalGrid(e.map(f))
      case VerticalGrid(e) => VerticalGrid(e.map(f))
      case Placement(i, c) => Placement(i, f(c))
    }
  }
}

trait Page[P] {
  def empty : P
  def image(src : String) : P
  def code(code : String) : P

  def horizontalSequence(elements : NonEmptyList[P]) : P
  def verticalSequence(s : style.VerticalSequence, elements : NonEmptyList[P]) : P

  def horizontalGrid(elements : Partition[P]) : P
  def verticalGrid(elements : Partition[P]) : P

  def placement(info : PlacementInfo, content : P) : P

  final def algebra(pa : PageF[P]) : P = pa match {
    case PageF.Empty => empty
    case PageF.Image(s) => image(s)
    case PageF.Code(s) => code(s)
    case PageF.Placement(i, c) => placement(i, c)
    case PageF.HorizontalSequence(e) => horizontalSequence(e)
    case PageF.VerticalSequence(s, e) => verticalSequence(s, e)
    case PageF.HorizontalGrid(e) => horizontalGrid(e)
    case PageF.VerticalGrid(e) => verticalGrid(e)
  }

  final val Algebra = new Algebra(algebra _)
}