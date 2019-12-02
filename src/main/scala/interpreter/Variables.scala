package interpreter

import parser.Statement
import typecheck.Type

sealed trait Variable
  case class Var(value : String, _type : Type) extends Variable
  case class Function(parameterTypes : List[Type], body : Statement, _type : Type) extends Variable
  case class SysCall(parameterTypes : List[Type], body : Function1 [Seq[String], Option[String]], _type : Type) extends Variable
