package CoSlide

package object utils {

  def withOpt[A, T](opt : Option[A], t : T)(f : (A , T) => T) : T = opt match {
    case Some(a) => f(a, t)
    case _       => t
  }
}
