package interpreter

import interpreter.PCodeInterpreter.evaluateExpression
import parser.{ArithmeticOperator, BooleanOperator, Statement}
import typecheck.Type

sealed trait Variable
  abstract class Comparable[T <% Ordered[T]](val value : T) extends Variable
  {
    def compare(op :BooleanOperator, other : Comparable[T]): Boolean = {
      val otherval = other.value : T

      op match {
        case parser.Gt() => value.>(otherval)
        case parser.Lt() => value.<(otherval)
        case parser.Eq() => value.equals(otherval)
        case parser.Neq() => !value.equals(otherval)
      }
    }
  }



  case class IntVar(override val value : Int) extends Comparable(value)
  case class StringVar(value : String) extends Variable
  case class FloatVar(override val value: Double) extends Comparable(value)
  case class BoolVar(value : Boolean) extends Variable
  case class Function(parameterTypes : List[Type], body : Statement, _type : Type) extends Variable
  case class SysCall(parameterTypes : List[Type], body : Function1 [Seq[String], Option[Variable]], _type : Type) extends Variable

