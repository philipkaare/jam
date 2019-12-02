package interpreter

import parser.{Expression, Program, Statement}
import typecheck.TNotSet

import scala.collection.immutable.HashMap


object PCodeInterpreter {

  def run(p :Program): Unit = {
    val state = SysCalls.initializeSyscalls(HashMap())
    executeStatements(p.statements.toList, state)
  }

  def executeStatements(statements : List[Statement], state: HashMap[String, Variable]): HashMap[String, Variable] = statements match  {
    case Nil => state
    case stat :: rest =>
      val updatedState = executeStatement(stat, state)
      executeStatements(rest, updatedState)
  }

  def executeStatement(stat : Statement, state : HashMap[String, Variable]) : HashMap[String, Variable] = {
    stat match {
      case parser.VarAssignment(varname, expression) =>
        state + (varname -> Var(evaluateExpression(expression, state).toString, TNotSet()))
      case parser.SubroutineCall(fcall) =>
        state.get(fcall.name) match {
          case Some(v) => v match {
            case SysCall(parameters, body, _type) =>
              body.apply(fcall.parameters.map(p => evaluateExpression(p, state).toString))
              state
          }
        }
      case parser.IfThenElse(_condition, _then, _else) =>
        if (evaluateExpression(_condition, state) == "true")
          executeStatements(_then.toList, state)
        else
          executeStatements(_else.toList, state)
    }
  }

  def evaluateExpression(expr : Expression, state: HashMap[String, Variable]) : String = {
    expr match {
      case parser.ExprValue(value) => value match {
        case parser.PInt(value1) => value1.toString
        case parser.PString(value1) => value1
        case parser.PFloat(value1) => value1.toString
      }
      case parser.Identifier(id) => state.get(id) match {
        case Some(Var(value, _type)) => value
        case None => throw new Exception("Variable " + id + " not defined when referenced. ")
      }
      case parser.Expr(op, l, r) => op match {
        case parser.Mult() => (evaluateExpression(l, state).toInt * evaluateExpression(r, state).toInt).toString
        case parser.Div() => (evaluateExpression(l, state).toInt / evaluateExpression(r, state).toInt).toString
        case parser.Plus() => (evaluateExpression(l, state).toInt + evaluateExpression(r, state).toInt).toString
        case parser.Minus() => (evaluateExpression(l, state).toInt - evaluateExpression(r, state).toInt).toString
        case parser.Gt() => (evaluateExpression(l, state).toInt > evaluateExpression(r, state).toInt).toString
        case parser.Lt() => (evaluateExpression(l, state).toInt < evaluateExpression(r, state).toInt).toString
        case parser.Eq() => (evaluateExpression(l, state).toInt == evaluateExpression(r, state).toInt).toString
        case parser.Neq() => (evaluateExpression(l, state).toInt != evaluateExpression(r, state).toInt).toString
      }
      case parser.FunctionCall(name, parameters) => {
        state.get(name) match {
          case Some(v) => v match {
            case SysCall(parameterTypes, body, _type) =>
              body.apply(parameters.map(p => evaluateExpression(p, state).toString)).get
          }
        }
      }
    }
  }
}
