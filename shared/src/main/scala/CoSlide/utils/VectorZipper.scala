package CoSlide.utils

import cats.data.NonEmptyList
import cats.functor.Bifunctor
import cats.~>

final case class VectorZipper[+A, +B](left : Vector[A], focus : B, right : Vector[A])

object VectorZipper {
  implicit val functorVectorZipper = new Bifunctor[VectorZipper] {
    def bimap[A, B, C, D](fab: VectorZipper[A, B])(f: A => C, g: B => D) : VectorZipper[C,D] =
      VectorZipper(fab.left.map(f), g(fab.focus), fab.right.map(f))
  }
}