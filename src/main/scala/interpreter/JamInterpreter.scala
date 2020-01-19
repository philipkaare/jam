package interpreter

import cats.implicits._
import cats.data.State
import cats.syntax.traverse
import exceptions.RuntimeTypeException
import interpreter.JamInterpreter.{evaluateExpression, executeStatement, executeStatements}
import parser._
import typecheck.TNotSet
import scala.collection.immutable.HashMap
import scala.math.Numeric.DoubleIsFractional


object JamInterpreter {

  def run(p :Program): Unit = {
    val state = SysCalls.initializeSyscalls().run(HashMap()).value._1
    executeStatements(p.statements.toList, state)
  }

  def executeStatements(statements : Seq[Statement], state : HashMap[String, Variable]): Unit = {
    (for {
        _ <- statements.toList.traverse(executeStatement)
      } yield ()).run(state).value._2
  }


  def executeStatement(stat : Statement) : State[HashMap[String, Variable], Unit] = {

        stat match {
          case FunctionDeclaration(name, parameters, body, returnType, loc) =>
            Base.updateState(name, Function(parameters, body, returnType))
          case parser.VarAssignmentAndDeclaration(varname, expression, loc) =>
            evaluateExpression(expression).flatMap(exprType => Base.updateState(varname, exprType))
          case VarAssignment(varname, expression, loc) =>
            evaluateExpression(expression).flatMap(exprType => Base.updateState(varname, exprType))
          case parser.SubroutineCall(fcall, loc) =>
            (for {
              function <- Base.getVariable[Variable](fcall.name)
              params <- fcall.parameters.toList.traverse(evaluateExpression)
            }
            yield (function, params))
            .flatMap({case (function, params) => State (s => {
              function match {
                case Some(x) => x match {
                  case SysCall(parameters, body, _type) =>
                    body.apply(params)
                  case Function(paramBindings, body, _type) =>
                    val pushedParams = paramBindings.map { case TypeBinding(varname, _) =>  varname}.zip(params).toMap : Map[String, Variable]
                    executeStatements(body, s ++ pushedParams)
                }
              }
              (s,())
            })
            })
          case parser.IfThenElse(_condition, _then, _else, loc) =>
            evaluateExpression(_condition).flatMap(cond =>
              State(s => {
                if (cond == BoolVar(true))
                  executeStatements(_then.toList, s)
                else
                  executeStatements(_else.toList, s)
                (s, ())
              }
          ))
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
      case parser.ExprValue(value, loc) => value match {
        case parser.PInt(value1) => State(s => (s,IntVar(value1)))
        case parser.PString(value1) => State(s => (s,StringVar(value1)))
        case parser.PFloat(value1) => State(s => (s,FloatVar(value1)))
        case parser.PBool(value1) => State(s => (s,BoolVar(value1)))
      }
      case parser.Identifier(id, loc) => for {
            v <- Base.getVariable(id)
          } yield{
            v match {
              case Some(variable) => variable
              case None => throw new Exception("Variable " + id + " not defined when referenced. ")
            }
          }
      case parser.Expr(op, l, r, loc) =>
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
      case parser.FunctionCall(name, parameters, loc) => {
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
