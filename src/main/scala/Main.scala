import exceptions.TypeCheckException
import interpreter.{Base, JamInterpreter}
import org.parboiled2.ParseError
import parser.JamParser
import typecheck.Typechecker

import scala.io.Source

object HelloWorld {
  def main(args: Array[String]): Unit = {

    val filename = "test.jam"
    var parser = null : JamParser
    try
    {
      println("Welcome to the Jam interpreter version 0.1 :)")
      println("Let's jam!")
      println(s"Opening $filename")
      val fileContents = Source.fromFile(filename).getLines.mkString("\n")
      Typechecker.getLineNo = Base.getLineNo(fileContents)
      println("Parsing phase ...")
      parser = new JamParser(fileContents)
      val res = parser.Input.run().get
      println("Parsing phase complete. ")
      println("Typechecking phase ...")
      Typechecker.check(res)
      println("Typechecking phase complete. ")
      println("Execution phase ...")
      JamInterpreter.run(res)
      println("Execution phase complete. ")
    }
    catch
    {
      case error : ParseError  => println(parser.formatError(error))
      case error : TypeCheckException => println(error.getMessage)
    }




  }
}