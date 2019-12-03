package typecheck

import exceptions.TypeCheckException
import interpreter.{SysCall, SysCalls, Variable}
import parser._

import scala.collection.immutable.HashMap

sealed trait Type
case class TString() extends Type
case class TInt() extends Type
case class TFloat() extends Type
case class TBool() extends Type
case class TUnit() extends Type
case class TNotSet() extends Type
case class TFunctionCall(params : List[Type], returnType : Type) extends Type

object Typechecker {

  def check(p :Program): Unit = {

    val state = (SysCalls initializeSyscalls HashMap()) map{
        case (name, variable) =>
            variable match {
              case SysCall(parameterTypes, body, _type) => (name, TFunctionCall(parameterTypes,_type))
              case _ => throw new TypeCheckException("A variable of type other than syscall in syscall collection. Bad, bad programmer!")
            }
      }
    checkStatements(p.statements.toList, state)
  }

  def checkStatements(statements : List[Statement], state: HashMap[String, Type]): HashMap[String,  Type] = statements match  {
    case Nil => state
    case stat :: rest =>
      val updatedState = checkStatement(stat, state)
      checkStatements(rest, updatedState)
  }

  def checkFunctionCall(name : String, parameters : Seq[Expression],  state : HashMap[String,  Type]) :  Type = {
    state.get(name) match {
      case Some(_type) => _type match {
        case  TFunctionCall(calleeParameters,returnType) =>

          val callParamTypes = parameters.map(p => checkExpression(p, state))

          if (callParamTypes != calleeParameters)
            throw new TypeCheckException("Parameter types did not match. ")
          returnType
      }
    }
  }

  def checkStatement(stat : Statement, state : HashMap[String,  Type]) : HashMap[String,  Type] = {
    stat match {
      case parser.VarAssignment(varname, expression) =>
        state + (varname -> checkExpression(expression, state))
      case parser.SubroutineCall(fcall) =>
        checkFunctionCall(fcall.name, fcall.parameters, state)
        state
      case parser.IfThenElse(_condition, _then, _else) =>
        if (!checkExpression(_condition, state).isInstanceOf[TBool])
          throw new TypeCheckException("Expression in if was boolean. ")
        val updatedState = checkStatements(_then.toList, state)
        checkStatements(_else.toList, state)
    }
  }

  def checkExpression(expr : Expression, state: HashMap[String,  Type]) :  Type = {
    expr match {
      case parser.ExprValue(value) => value match {
        case parser.PInt(_) => TInt()
        case parser.PString(_) =>  TString()
        case parser.PFloat(_) =>  TFloat()
      }
      case parser.Identifier(id) => state.get(id) match {
        case Some( _type) => _type
        case None => throw new Exception("Variable " + id + " not defined when referenced. ")
      }
      case parser.Expr(op, l, r) =>
        val leftType = checkExpression(l, state)
        val rightType = checkExpression(r, state)
        if (leftType != rightType)
          throw new Exception(s"Type mismatch in expression: $leftType not equal to $rightType")
        else
          op match {
            case _ : ArithmeticOperator => leftType
            case _ : BooleanOperator => //todo: Does it make sense to compare strings with < / > ?
              TBool()
          }
      case parser.FunctionCall(name, parameters) =>
        checkFunctionCall(name, parameters, state)
    }
  }
}
