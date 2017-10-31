package CoSlide

package object utils {

  def withOpt[A, T](opt : Option[A], t : T)(f : (A , T) => T) : T = opt match {
    case Some(a) => f(a, t)
    case _       => t
  }

  /** @param d a percentagle in [[Double]] form 0 <= d <= 1
    * @return this percentage a string "n%" with 0 <= n <= 100
    */
  def percent(d : Double) : String = s"($d * 100).toInt%"
}
