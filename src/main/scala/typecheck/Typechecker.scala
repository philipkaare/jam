package typecheck

import interpreter.{SysCall, SysCalls, Var, Variable}
import parser.{Expression, FunctionCall, Program, Statement}

import scala.collection.immutable.HashMap

object Typechecker {

  def check(p :Program): Unit = {
    val state = SysCalls.initializeSyscalls(HashMap())
    checkStatements(p.statements.toList, state)
  }

  def checkStatements(statements : List[Statement], state: HashMap[String, Variable]): Unit = statements match  {
    case Nil => ()
    case stat :: rest =>
      val updatedState = checkStatement(stat, state)
      checkStatements(rest, updatedState)
  }

  def checkFunctionCall(name : String, parameters : Seq[Expression],  state : HashMap[String, Variable]) : String = {
    state.get(name) match {
      case Some(variable) => variable match {
        case SysCall(calleeParameters, body, _type) =>
          val callParamTypes = parameters.map(p => checkExpression(p, state))
          if (callParamTypes != calleeParameters)
            throw new Exception(s"Call to: $name had mismatch of parameters. Was: ${callParamTypes.toString()}, expected $calleeParameters. ")
          _type
      }
    }
  }

  def checkStatement(stat : Statement, state : HashMap[String, Variable]) : HashMap[String, Variable] = {
    stat match {
      case parser.VarAssignment(varname, expression) =>
        state + (varname -> Var("unset", checkExpression(expression, state)))
      case parser.SubroutineCall(fcall) =>
        checkFunctionCall(fcall.name, fcall.parameters, state)
        state
    }
  }


  def checkExpression(expr : Expression, state: HashMap[String, Variable]) : String = {
    expr match {
      case parser.ExprValue(value) => value match {
        case parser.PInt(_) => "int"
        case parser.PString(_) => "string"
        case parser.PDouble(_) => "double"
      }
      case parser.Identifier(id) => state.get(id) match {
        case Some(Var(value, _type)) => _type
        case None => throw new Exception("Variable " + id + " not defined when referenced. ")
      }
      case parser.Expr(op, l, r) =>
        val leftType = checkExpression(l, state)
        val rightType = checkExpression(r, state)
        if (leftType != rightType)
          throw new Exception(s"Type mismatch in expression: $leftType not equal to $rightType")
        else
          leftType
      case parser.FunctionCall(name, parameters) =>
        checkFunctionCall(name, parameters, state)
    }
  }
}
