import CoSlide.utils._

package object CoSlide {
  type VTextF[+S, +T] = Vector[TextF[S,T]]

  type TextOutF[+A] = VTextF[style.TextOut, A]
  type TextOut = LFix[TextOutF]

  type PageOutF[+A] = Either[TextOut, PageF[A]]
  type PageOut = LFix[PageOutF]

  type TableOfContents[+T] = TableOfContentsF[Vector, T]
  type TableOfContentsHole[+T] = TableOfContentsF[VectorZipper, T]
}
