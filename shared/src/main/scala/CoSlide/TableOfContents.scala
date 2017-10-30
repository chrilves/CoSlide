package CoSlide

import cats.{Functor, ~>}

final case class TableOfContentsF[F[+_],+T](sections : F[TableOfContentsF.Section[F, T]]) {
  import cats.syntax.functor._

  def bimap[G[+_] : Functor, U](nt : F ~> G, f : T => U) : TableOfContentsF[G, U] =
    TableOfContentsF[G,U](nt(sections).map(_.bimap(nt, f)))
}
object TableOfContentsF {
  final case class  Section[F[+_],+T](title : T, subSections : TableOfContentsF[F, T]) {
    def bimap[G[+_] : Functor, U](nt : F ~> G, f : T => U) : Section[G, U] =
      Section[G,U](f(title), subSections.bimap(nt, f))
  }
}
