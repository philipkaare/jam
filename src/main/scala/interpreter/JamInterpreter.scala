package interpreter

import cats.arrow.FunctionK
import cats.implicits._
import cats.data.{EitherT, State, StateT}
import cats.syntax.traverse
import cats.~>
import exceptions.RuntimeTypeException
import interpreter.Base.Result
import interpreter.JamInterpreter.{evaluateExpression, executeStatement, executeStatements}
import parser._
import typecheck.TNotSet
import util.Util.PipeEverything

import scala.collection.immutable.HashMap
import scala.math.Numeric.DoubleIsFractional

object JamInterpreter {
  type StatefulResult = Base.StatefulResult[Variable, Variable]

  def run(p :Program): Either[String, Unit] = {
    val result = (for {
      _<-SysCalls.initializeSyscalls()
      r<-executeStatements(p.statements.toList)
    }yield r).run(HashMap()).value

    result match {
      case Some(result) =>
        result match {
          case Left(msg) => Left(msg)
          case Right(_) => Right()
        }
      case _ => Right()
    }
  }
//
//  def flatMapOption(res : StatefulResult, f : (Base.Result[Variable]=> StatefulResult)) : StatefulResult=
//    StateT(s =>{
//      val ret : Result[Variable] = EitherT.pure(None)
//      res.run(s).value match {
//        case Some(x) => x match {
//          case Left(msg) =>
//          case Right((state, v)) => f(ret).run(x)
//        }
//        case None => f(ret)
//      }
//    }
//    )

  def error(msg : String) : StatefulResult = StateT.liftF(EitherT.leftT(msg))

  def result(v : Variable) : StatefulResult = StateT.liftF(EitherT.rightT(v))

  def emptyResult : StatefulResult = StateT.liftF(EitherT.rightT(UnitVar()))

  def executeStatements(statements : Seq[Statement]): StatefulResult = {
    statements.toList.traverse(executeStatement).flatMap(r => result(r.last))
  }

  def assignVariable(varname:String, expression: Expression) : StatefulResult  = {
    for {
      exprResult <- evaluateExpression(expression)
      _ <- Base.updateState(varname, exprResult)
    } yield UnitVar()
  }

  def executeFunctionCall(name : String, parameters : Seq[Expression]) : StatefulResult= {
    def exec : (Variable, List[Variable]) => StatefulResult = (function : Variable, params : List[Variable]) => {
      function match {
        case SysCall(_, body, _) =>
          StateT.liftF(EitherT.fromEither(body.apply(params)))
        case Function(paramBindings, body, _) =>
          (for {
            state <- Base.getState[Variable]()
            pushedParams = paramBindings.map { case TypeBinding(varname, _) => varname }.zip(params).toMap: Map[String, Variable]
          } yield (state ++ pushedParams)) >>=
            (state => {
              executeStatements(body).run(state).value match {
                case Some(Right((_, v))) => result(v)
                case Some(Left(msg)) => error(msg)
                case _ => error("Empty result when executing function! ")
              }})
      } }

    for {
      function <- Base.getVariable[Variable](name)
      params <- parameters.toList.traverse(evaluateExpression)
      res <- exec(function, params)
    } yield res
  }

  def executeStatement(stat : Statement) : StatefulResult = {
      stat match {
        case FunctionDeclaration(name, parameters, body, returnType, loc) =>
          (Base.updateState[Variable](name, Function(parameters, body, returnType)) *> emptyResult)
             //>>= (_ => emptyResult))
        case parser.VarAssignmentAndDeclaration(varname, expression, loc) =>
          assignVariable(varname, expression)
        case VarAssignment(varname, expression, loc) =>
          assignVariable(varname, expression)
        case parser.SubroutineCall(fcall, loc) =>
          executeFunctionCall(fcall.name,fcall.parameters )
        case parser.IfThenElse(_condition, _then, _else, loc) =>
          evaluateExpression(_condition).flatMap(exprVal => {
              if (exprVal == BoolVar(true))
                executeStatements(_then.toList)
              else
                executeStatements(_else.toList)
          })
        case parser.WhileLoop(_condition, _body, _) =>
          def whileLoop() : StatefulResult = {
            evaluateExpression(_condition).flatMap(exprVal => {
              if (exprVal == BoolVar(true))
                executeStatements(_body).flatMap(_ => whileLoop())
              else
                emptyResult
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


  def evaluateExpression(expr : Expression) : StatefulResult = {
    expr match {
      case parser.ExprValue(value, loc) => value match {
        case parser.PInt(value1) => StateT.pure(IntVar(value1))
        case parser.PString(value1) => StateT.pure(StringVar(value1))
        case parser.PFloat(value1) => StateT.pure(FloatVar(value1))
        case parser.PBool(value1) => StateT.pure(BoolVar(value1))
      }
      case parser.Identifier(id, loc) =>
        Base.getVariable(id)
      case parser.Expr(op, l, r, loc) =>
        (for {
          leftvar <- evaluateExpression(l)
          rightvar <- evaluateExpression(r)
        }yield ((leftvar,rightvar))) >>= (vars => {
          op match {
            case boolOp: BooleanOperator =>
              (vars._1, vars._2) match {
                case (lcomp: Comparable[Ordered[Int]], rcomp: Comparable[Ordered[Int]]) =>
                  StateT.pure(BoolVar(lcomp.compare(boolOp, rcomp)))
                case _ => StateT.liftF(EitherT.leftT("Expected a comparable expression with boolean operator, but was not comparable. The typechecking phase should have caught this..."))
              }
            case arithOp: ArithmeticOperator =>
              (vars._1, vars._2) match {
                case (lint: IntVar, rint: IntVar) => StateT.pure(calculate(arithOp, lint, rint))
                case (lfloat: FloatVar, rfloat: FloatVar) => StateT.pure(calculate(arithOp, lfloat, rfloat))
                case _ => StateT.liftF(EitherT.leftT("Expected a numeric type. The typechecking phase should have caught this..."))
              }
          }})
      case parser.FunctionCall(name, parameters, loc) =>
        executeFunctionCall(name, parameters)
    }
  }
}
