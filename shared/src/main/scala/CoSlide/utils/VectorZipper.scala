package CoSlide.utils

import cats.data.NonEmptyList
import cats.~>

final case class VectorZipper[+A](left : Vector[A], focus : A, right : Vector[A]) {
  def map[B](f : A => B) : VectorZipper[B] =
    VectorZipper(left.map(f), f(focus), right.map(f))

  def toVector : Vector[A] = left ++ (focus +: right)
}

object VectorZ extends (VectorZipper ~> Vector) {
  def apply[A](fa: VectorZipper[A]) : Vector[A] = fa.toVector
}