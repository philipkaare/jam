import interpreter.PCodeInterpreter
import org.parboiled2.ParseError
import parser.PCodeParser
import typecheck.Typechecker

import scala.io.Source

object HelloWorld {
  def main(args: Array[String]): Unit = {

    var parser = null : PCodeParser
    try
    {
      val fileContents = Source.fromFile("test.pc").getLines.mkString("\n")

      parser = new PCodeParser(fileContents)

      val res = parser.Input.run()

      Typechecker.check(res.get)
      PCodeInterpreter.run(res.get)
    }
    catch
    {
      case error : ParseError  => print(parser.formatError(error))
    }




  }
}