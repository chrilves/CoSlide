package CoSlide

import cats.Functor
import cats.syntax.functor._

///////////////////////////////////////////////////
//
//  Algebra
//

final class Algebra[F[_], A](val fold : F[A] => A) extends AnyVal
object Algebra {
  def apply[F[_], A](implicit alg : Algebra[F,A]) = alg
}

// Least Fixed Point
final case class LFix[F[_]](out : F[LFix[F]]) {
  def fold[A](implicit F : Functor[F], alg : Algebra[F,A]) : A =
    alg.fold(out.map(_.fold[A](F,alg)))
}

object LFix {
  def algebra[F[_]](fa : F[LFix[F]]) : LFix[F] = LFix[F](fa)
  def Algebra[F[_]] : Algebra[F, LFix[F]] = new Algebra[F, LFix[F]](algebra _)
}


///////////////////////////////////////////////////
//
//  CoAlgebra
//

final class CoAlgebra[F[_],A](val unfold : A => F[A]) extends AnyVal
object CoAlgebra {
  def apply[F[_],A](implicit coalg : CoAlgebra[F,A]) = coalg
}

// Greated Fixed Point
trait GFix[F[_]] {
  def unfold : F[GFix[F]]
}

object GFix {

  def unfold[F[_]: Functor, A](a : A)(implicit coalg : CoAlgebra[F,A]) : GFix[F] = {
    def aux(a2 : A) : GFix[F] = new GFix[F] {
      lazy val unfold : F[GFix[F]] = coalg.unfold(a2).map(aux)
    }

    aux(a)
  }

  implicit final class GFixOps[A](val self : A) extends AnyVal {
    def unfold[F[_]: Functor](implicit coalg : CoAlgebra[F,A]) : GFix[F] = GFix.unfold(self)
  }
}

///////////////////////////////////////////////////
//
//  Inject
//

trait Inject[A, B] {
  def inject(a : A) : B
}
