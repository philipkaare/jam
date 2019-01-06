import parser.PCodeParser

object HelloWorld {
  def main(args: Array[String]): Unit = {

    val res = new PCodeParser("var x<-\"test\" \n").Input.run()

    print(res)
  }
}