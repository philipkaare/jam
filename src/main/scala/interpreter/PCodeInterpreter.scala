package interpreter

import exceptions.RuntimeTypeException
import parser._
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
        state + (varname -> evaluateExpression(expression, state))
      case parser.SubroutineCall(fcall) =>
        state.get(fcall.name) match {
          case Some(v) => v match {
            case SysCall(parameters, body, _type) =>
              body.apply(fcall.parameters.map(p => evaluateExpression(p, state).toString))
              state
          }
        }
      case parser.IfThenElse(_condition, _then, _else) =>
        if (evaluateExpression(_condition, state) == BoolVar(true))
          executeStatements(_then.toList, state)
        else
          executeStatements(_else.toList, state)
    }
  }



  def evaluateExpression(expr : Expression, state: HashMap[String, Variable]) : Variable = {

    expr match {
      case parser.ExprValue(value) => value match {
        case parser.PInt(value1) => IntVar(value1)
        case parser.PString(value1) => StringVar(value1)
        case parser.PFloat(value1) => FloatVar(value1)
        case parser.PBool(value1) => BoolVar(value1)
      }
      case parser.Identifier(id) => state.get(id) match {
        case Some(variable) => variable
        case None => throw new Exception("Variable " + id + " not defined when referenced. ")
      }
      case parser.Expr(op, l, r) =>
        val leftvar = evaluateExpression(l, state)
        val rightvar = evaluateExpression(r, state)

        op match {
          case boolOp: BooleanOperator =>
            (leftvar, rightvar) match {
              case (lcomp: Comparable[Ordered[Int]], rcomp: Comparable[Ordered[Int]]) =>
                BoolVar(lcomp.compare(boolOp, rcomp))
              case _ => throw new RuntimeTypeException("Expected a comparable expression with boolean operator, but was not comparable. The typechecking phase should have caught this...")
            }
          case arithOp : ArithmeticOperator =>
            IntVar(1)
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
