import interpreter.PCodeInterpreter
import org.parboiled2.ParseError
import parser.PCodeParser
import typecheck.Typechecker

import scala.io.Source

object HelloWorld {
  def main(args: Array[String]): Unit = {

    val filename = "test.pc"
    var parser = null : PCodeParser
    try
    {
      println(s"Opening $filename")
      val fileContents = Source.fromFile(filename).getLines.mkString("\n")
      println("Parsing phase ...")
      parser = new PCodeParser(fileContents)
      val res = parser.Input.run()
      println("Parsing phase complete. ")
      println("Typechecking phase ...")
      Typechecker.check(res.get)
      println("Typechecking phase complete. ")
      println("Execution phase ...")
      PCodeInterpreter.run(res.get)
      println("Execution phase complete. ")
    }
    catch
    {
      case error : ParseError  => println(parser.formatError(error))
    }




  }
}