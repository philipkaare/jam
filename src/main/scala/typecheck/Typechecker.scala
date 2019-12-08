package typecheck

import cats.data.State
import exceptions.TypeCheckException
import interpreter.{Base, SysCall, SysCalls, Variable}
import parser._
import cats.implicits._
import cats.syntax.traverse
import typecheck.Typechecker.{checkExpression, checkStatements}

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
    val init = SysCalls.initializeSyscalls().run(HashMap())
    val state = init.value._1.map{
        case (name, variable) =>
            variable match {
              case SysCall(parameterTypes, body, _type) => (name, TFunctionCall(parameterTypes,_type))
              case _ => throw new TypeCheckException("A variable of type other than syscall in syscall collection. Bad, bad programmer!")
            }
      }
    checkStatements(p.statements.toList).run(state).value
  }

  def checkStatements(statements : List[Statement]): State[HashMap[String,  Type],Unit] =
    statements match  {
    case Nil => State(s => (s, ()))
    case stat :: rest =>
      for {
        _ <- checkStatement(stat)
        _ <- checkStatements(rest)
      } yield ()
  }

  def checkFunctionCall(name : String, parameters : Seq[Expression]) : State[HashMap[String,  Type],Type]  = {
    def getFunction() : State[HashMap[String,  Type], Tuple3[Seq[Expression], Type, List[Type]]] = {
      for {
        functionReturnType <- Base.getVariable(name)
      }
        yield {
          functionReturnType match {
            case Some(_type) => _type match {
              case TFunctionCall(calleeParameters, returnType) =>
                (parameters, returnType, calleeParameters)
            }
          }
        }
    }

    for {
      function <- getFunction()
      parameters = function._1
      calleeParameters = function._3
      returnType = function._2
      callParamTypes <- parameters.toList.traverse(checkExpression)
    }
    yield {
      if (callParamTypes != calleeParameters)
        throw new TypeCheckException("Parameter types did not match. ")

      returnType
    }

  }

  def checkStatement(stat : Statement) : State[HashMap[String,  Type],Unit]  = {
    stat match {
      case parser.VarAssignment(varname, expression) =>
        for {
          t <- checkExpression(expression)
          _ <- Base.updateState(varname, t)
        }
        yield ()

      case parser.SubroutineCall(fcall) =>
        checkFunctionCall(fcall.name, fcall.parameters).flatMap(x => State(s => (s, ())))
      case parser.IfThenElse(_condition, _then, _else) =>
        (for {
          condType <- checkExpression(_condition)
        }
        yield {
          if (!condType.isInstanceOf[TBool])
            throw new TypeCheckException("Expression in if was not boolean. ")
          ()
        }).flatMap(x => {
          State(s =>{
            checkStatements(_then.toList).run(s).value
            checkStatements(_else.toList).run(s).value
            (s, ())
          })
        })

    }
  }

  def checkExpression(expr : Expression) : State[HashMap[String,  Type],Type] = {
    expr match {
      case parser.ExprValue(value) => value match {
        case PInt(_) => State(s => (s,TInt()))
        case PString(_) =>  State(s => (s,TString()))
        case PFloat(_) =>  State(s => (s,TFloat()))
        case PBool(_)=> State(s => (s, TBool()))
      }
      case parser.Identifier(id) =>
        for {
          v <- Base.getVariable(id)
        } yield{
          v match {
            case Some( _type) => _type
            case None => throw new Exception("Variable " + id + " not defined when referenced. ")
        }
      }
      case parser.Expr(op, l, r) =>
        for {
          leftType <- checkExpression(l)
          rightType <- checkExpression(r)

        }
        yield {
          if (leftType != rightType)
            throw new Exception(s"Type mismatch in expression: $leftType not equal to $rightType")
          else
            op match {
              case _ : ArithmeticOperator => leftType
              case _ : BooleanOperator => //todo: Does it make sense to compare strings with < / > ?
                TBool()
            }
        }

      case parser.FunctionCall(name, parameters) =>
        checkFunctionCall(name, parameters)
    }
  }
}
