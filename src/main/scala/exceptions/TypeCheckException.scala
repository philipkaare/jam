package exceptions

class TypeCheckException (message : String, loc : String) extends Exception(message) {
  override def getMessage: String = {
    if (loc != "")
      s"$loc: $message"
    else
      message
  }
}