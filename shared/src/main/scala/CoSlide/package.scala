import CoSlide.utils._
import cats.data.NonEmptyList

package object CoSlide {
  type VTextF[+S, +T] = Vector[TextF[S,T]]

  type TextOutF[+A] = VTextF[style.TextOut, A]
  type TextOut = LFix[TextOutF]

  type PageOutF[+A] = Either[TextOut, PageF[A]]
  type PageOut = LFix[PageOutF]

  type VTableOfContentsF[+T, +R] = Vector[TableOfContentsF[T,R]]
  type VTableOfContents[T] = LFix[VTableOfContentsF[T, ?]]
}
