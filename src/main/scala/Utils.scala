import scala.util.{Failure, Success, Try}

object Utils {
  def parseList[T](parser: String => Try[T], str: String): Try[Seq[T]] =
    if (str.isEmpty)
      Try(Seq())
    else {
      val asStrings = str.split("[\\s]*,[\\s]*")
      val asTrys = asStrings.map(parser(_))
      sequence(asTrys)
    }

//  https://stackoverflow.com/questions/15495678/flatten-scala-try
  def sequence[T](xs: Seq[Try[T]]): Try[Seq[T]] =
    xs.foldLeft(Try(Seq[T]()))((a, b) => a flatMap (c => b map (d => c :+ d)))
}
