package CoSlide.globals

sealed abstract class Text
object Text {
  final case object Author extends Text
  final case object Date   extends Text
  final case object Venue  extends Text
  final case object Title  extends Text
}

sealed abstract class Slide
object Slide {
  final case object TableOfContents extends Text
  final case object Title extends Text
}