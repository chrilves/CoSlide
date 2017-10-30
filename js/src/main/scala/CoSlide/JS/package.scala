package CoSlide

import scalatags.JsDom.all._

package object JS {
  implicit val textHTMLElement : Text[CoSlide.style.TextOut, Seq[Modifier]] = TextHTML
}
