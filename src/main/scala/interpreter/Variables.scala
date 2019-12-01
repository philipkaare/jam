package interpreter

import parser.Statement

sealed trait Variable
  case class Var(value : String, _type : String) extends Variable
  case class Function(parameterTypes : List[String], body : Statement, _type : String) extends Variable
  case class SysCall(parameterTypes : List[String], body : Function1 [Seq[String], Option[String]], _type : String) extends Variable
