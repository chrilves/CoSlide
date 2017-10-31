import cats.Functor
import cats.functor.Bifunctor
import cats.syntax.bifunctor._

package object CoSlide {
  type VTextF[+S, +T] = Vector[TextF[S,T]]

  type TextOutF[+A] = VTextF[style.TextOut, A]
  type TextOut = LFix[TextOutF]

  type PageOutF[+A] = PageF[A] with Inject[TextOut, A]
  type PageOut = LFix[PageOutF]

  type VTableOfContentsF[+T, +R] = Vector[TableOfContentsF[T,R]]
  type VTableOfContents[T] = LFix[VTableOfContentsF[T, ?]]

  implicit def bifunctorRight[F[_,_] : Bifunctor, L] = new Functor[F[L, ?]] {
    def map[A, B](fa: F[L,A])(f: A => B) : F[L,B] = fa.bimap(identity[L], f)
  }

  implicit def bifunctorLeft[F[_,_] : Bifunctor, R] = new Functor[F[?,R]] {
    def map[A, B](fa: F[A,R])(f: A => B) : F[B,R] = fa.bimap(f, identity[R])
  }

  implicit val btextfBifunctor = new Bifunctor[VTextF] {
    def bimap[A, B, C, D](fab: VTextF[A, B])(f: A => C, g: B => D) : VTextF[C,D] =
      fab.map(_.bimap(f,g))
  }
}
