import exceptions.TypeCheckException
import interpreter.{Base, JamInterpreter}
import org.parboiled2.ParseError
import parser.JamParser
import typecheck.Typechecker

import scala.io.Source

object HelloWorld {

  def parse(filename : String) = {
    var parser: JamParser = null
    try {
      val fileContents = Source.fromFile(filename).getLines.mkString("\n")
      Typechecker.getLineNo = Base.getLineNo(fileContents)
      println("Parsing phase ...")
      parser = new JamParser(fileContents)
      val res = parser.Input.run().get
      Right(res)
    }
    catch
    {
      case error : ParseError  => Left(parser.formatError(error))
      case error : TypeCheckException => Left(error.getMessage)
    }


  }

  def main(args: Array[String]): Unit = {

    val filename = "test.jam"

    println("Welcome to the Jam interpreter version 0.1 :)")
    println("Let's jam!")
    println(s"Opening $filename")
    val result = for {
      parseRes <- parse(filename)
      _ = println("Parsing phase complete. ")
      _ = println("Typechecking phase ...")
      _ <- Typechecker.check(parseRes)
      _ = println("Typechecking phase complete. ")
      _ = println("Execution phase ...")
      _ <- JamInterpreter.run(parseRes)
      _ = println("Execution phase complete. ")
    } yield()
    result match {
      case Left(msg) => println(msg)
      case _ => println("Executed ok. ")
    }
  }
}