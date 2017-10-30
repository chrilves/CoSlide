package CoSlide

sealed abstract class Orientation
final case object Next extends Orientation
final case object Previous extends Orientation

sealed abstract class Direction
object Direction {
  final case class Page(orientation: Orientation) extends Direction
  final case class Section(orientation: Orientation) extends Direction
  final case class Parent(orientation: Orientation) extends Direction
}

sealed abstract class Position

