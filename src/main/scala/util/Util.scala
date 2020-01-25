package util

object Util {
  implicit class PipeEverything[A](val underlying: A) extends AnyVal {
    def |>[B](f: A => B) = f(underlying)
  }
}
