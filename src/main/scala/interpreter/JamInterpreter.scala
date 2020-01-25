package interpreter

import cats.implicits._
import cats.data.State
import cats.syntax.traverse
import exceptions.RuntimeTypeException
import interpreter.JamInterpreter.{evaluateExpression, executeStatement, executeStatements}
import parser._
import typecheck.TNotSet
import util.Util.PipeEverything

import scala.collection.immutable.HashMap
import scala.math.Numeric.DoubleIsFractional


object JamInterpreter {

  def run(p :Program): Unit = {
    val state = SysCalls.initializeSyscalls().run(HashMap()).value._1
    executeStatements(p.statements.toList).run(state).value._2
  }

  def executeStatements(statements : Seq[Statement]): State[HashMap[String, Variable], Option[Variable]] = {
    for {
      stats <- statements.toList.traverse(executeStatement)
    } yield (stats.last)
  }

  def executeStatement(stat : Statement) : State[HashMap[String, Variable], Option[Variable]] = {
      stat match {
        case FunctionDeclaration(name, parameters, body, returnType, loc) =>
          Base.updateState[Variable](name, Function(parameters, body, returnType)) >>= Base.toNone()
        case parser.VarAssignmentAndDeclaration(varname, expression, loc) =>
          (evaluateExpression(expression)
            >>= (exprType => Base.updateState(varname, exprType))
            >>= Base.toNone())
        case VarAssignment(varname, expression, loc) =>
          (evaluateExpression(expression)
            >>= (exprType => Base.updateState(varname, exprType)
            >>= Base.toNone()))
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
                  executeStatements(body).run(s ++ pushedParams).value
              }
            }
            (s,None)
          })
          })
        case parser.IfThenElse(_condition, _then, _else, loc) =>
          evaluateExpression(_condition).flatMap(exprVal => {
              if (exprVal == BoolVar(true))
                executeStatements(_then.toList)
              else
                executeStatements(_else.toList)
          })
        case parser.WhileLoop(_condition, _body, _) =>
          def whileLoop() : State[HashMap[String, Variable], Option[Variable]] = {
            evaluateExpression(_condition).flatMap(exprVal => {
              if (exprVal == BoolVar(true))
                executeStatements(_body).flatMap(_ => whileLoop())
              else
                State(s => (s, None))
            })
          }
          whileLoop()
        case parser.Return(varname, loc) =>
          Base.getVariable(varname)
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
        case parser.PInt(value1) => IntVar(value1) |> State.pure
        case parser.PString(value1) => StringVar(value1)|> State.pure
        case parser.PFloat(value1) => FloatVar(value1)|> State.pure
        case parser.PBool(value1) => BoolVar(value1)|> State.pure
      }
      case parser.Identifier(id, loc) =>
        for {
          v <- Base.getVariable(id)
        }yield {
          v match {
            case Some(variable) => variable
            case None => throw new Exception("Variable " + id + " not defined when referenced. ")
          }
        }
      case parser.Expr(op, l, r, loc) =>
        for {
          leftvar <- evaluateExpression(l)
          rightvar <- evaluateExpression(r)
        }yield {
          op match {
            case boolOp: BooleanOperator =>
              (leftvar, rightvar) match {
                case (lcomp: Comparable[Ordered[Int]], rcomp: Comparable[Ordered[Int]]) =>
                  BoolVar(lcomp.compare(boolOp, rcomp))
                case _ => throw new RuntimeTypeException("Expected a comparable expression with boolean operator, but was not comparable. The typechecking phase should have caught this...")
              }
            case arithOp: ArithmeticOperator =>
              (leftvar, rightvar) match {
                case (lint: IntVar, rint: IntVar) => calculate(arithOp, lint, rint)
                case (lfloat: FloatVar, rfloat: FloatVar) => calculate(arithOp, lfloat, rfloat)
                case _ => throw new RuntimeTypeException("Expected a numeric type. The typechecking phase should have caught this...")
              }
          }
        }
      case parser.FunctionCall(name, parameters, loc) =>
        (for {
          v <- Base.getVariable[Variable](name)
          params <- parameters.toList.traverse(evaluateExpression)
        } yield {
          v match {
            case Some(v) => (v, params)
          }
        }).flatMap({ case (v, params) => {
          v match {
            case SysCall(parameterTypes, body, _type) =>
              State(s => (s,body.apply(params).get))
            case Function(paramBindings, body, _type) =>
              val pushedParams = paramBindings.map { case TypeBinding(varname, _) =>  varname}.zip(params).toMap : Map[String, Variable]

              State(s  => {
                val x = executeStatements(body).run(s ++ pushedParams).value._2.get
                (s, x)
              })
          }
        }})

    }
  }
}
