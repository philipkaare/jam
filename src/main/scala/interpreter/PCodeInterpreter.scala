package interpreter

import cats.implicits._
import cats.data.State
import cats.syntax.traverse
import exceptions.RuntimeTypeException
import interpreter.PCodeInterpreter.{evaluateExpression, executeStatement, executeStatements}
import parser._
import typecheck.TNotSet
import scala.collection.immutable.HashMap
import scala.math.Numeric.DoubleIsFractional


object PCodeInterpreter {

  def run(p :Program): Unit = {
    val state = SysCalls.initializeSyscalls().run(HashMap()).value
    executeStatements(p.statements.toList).run(state._1).value
  }

  def executeStatements(statements : List[Statement]): State[HashMap[String, Variable], Unit] = statements match  {
    case Nil => State(s => (s, ()))
    case stat :: rest =>
      for {
        _ <- executeStatement(stat)
        _ <- executeStatements(rest)
      } yield ()
  }


  def executeStatement(stat : Statement) : State[HashMap[String, Variable], Unit] = {

        stat match {
          case parser.VarAssignment(varname, expression) =>
            evaluateExpression(expression).flatMap(exprType => Base.updateState(varname, exprType))
          case parser.SubroutineCall(fcall) =>
            for {
              v <- Base.getVariable(fcall.name)
              params <- fcall.parameters.toList.traverse(evaluateExpression)
            }
            yield {
              v match {
                case Some(x) => x match {
                  case SysCall(parameters, body, _type) =>
                    body.apply(params)
                }
              }
              ()
            }
          case parser.IfThenElse(_condition, _then, _else) =>
            for {
              cond <- evaluateExpression(_condition)

              s <- if (cond == BoolVar(true))
                executeStatements(_then.toList)
              else
                executeStatements(_else.toList)
            } yield ()


        }
  }

  def calculate(arithOp : ArithmeticOperator, x: IntVar, y : IntVar) : IntVar = {

    IntVar(arithOp match {
      case Mult() => x.value * y.value
      case Plus() => x.value + y.value
      case Minus() => x.value - y.value
      case Div() => x.value / y.value
    })
  }

  def calculate(arithOp : ArithmeticOperator, x: FloatVar, y : FloatVar) : FloatVar = {

    FloatVar(arithOp match {
      case Mult() => x.value * y.value
      case Plus() => x.value + y.value
      case Minus() => x.value - y.value
      case Div() => x.value / y.value
    })
  }


  def evaluateExpression(expr : Expression) : State[HashMap[String, Variable], Variable] = {

    expr match {
      case parser.ExprValue(value) => value match {
        case parser.PInt(value1) => State(s => (s,IntVar(value1)))
        case parser.PString(value1) => State(s => (s,StringVar(value1)))
        case parser.PFloat(value1) => State(s => (s,FloatVar(value1)))
        case parser.PBool(value1) => State(s => (s,BoolVar(value1)))
      }
      case parser.Identifier(id) => for {
            v <- Base.getVariable(id)
          } yield{
            v match {
              case Some(variable) => variable
              case None => throw new Exception("Variable " + id + " not defined when referenced. ")
            }
          }
      case parser.Expr(op, l, r) =>
        for {
          leftvar <- evaluateExpression(l)
          rightvar <- evaluateExpression(r)
        }
        yield{
          op match {
            case boolOp: BooleanOperator =>
              (leftvar, rightvar) match {
                case (lcomp: Comparable[Ordered[Int]], rcomp: Comparable[Ordered[Int]]) =>
                  BoolVar(lcomp.compare(boolOp, rcomp))
                case _ => throw new RuntimeTypeException("Expected a comparable expression with boolean operator, but was not comparable. The typechecking phase should have caught this...")
              }
            case arithOp : ArithmeticOperator =>
              (leftvar, rightvar) match {
                case (lint: IntVar, rint: IntVar) => calculate(arithOp, lint, rint)
                case (lfloat : FloatVar, rfloat : FloatVar) => calculate(arithOp, lfloat, rfloat)
                case _ => throw new RuntimeTypeException("Expected a numeric type. The typechecking phase should have caught this...")
              }
          }
        }
      case parser.FunctionCall(name, parameters) => {
        for {
          v <- Base.getVariable(name)
          params <- parameters.toList.traverse(evaluateExpression)
        }
        yield{
          v match {
            case Some(v) => v match {
              case SysCall(parameterTypes, body, _type) =>
                body.apply(params).get
            }
          }
        }
      }
    }
  }
}
