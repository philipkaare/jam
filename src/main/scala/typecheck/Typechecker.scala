package typecheck

import cats.Functor
import cats.data.State
import exceptions.TypeCheckException
import interpreter.{Base, SysCall, SysCalls, Variable}
import parser._
import cats.implicits._
import cats.data._
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

  def toString(t : Type) : String = {
    t match {
      case TString() => "String"
      case TInt() => "Int"
      case TFloat() => "Float"
      case TBool() => "Boolean"
      case TUnit() => "Unit"
      case TNotSet() => "Unset"
      case TFunctionCall(params, returnType) =>
          "(" + toString(params) + ") => " + toString(returnType)
    }
  }

  def toString(tl : Seq[Type]) : String = "("+ tl.map(t => toString(t)).reduce((l,r)=>l+","+r) + ")"


  var getLineNo : Int => String = s => ""

  def check(p :Program): Unit = {
    val init = SysCalls.initializeSyscalls().run(HashMap())
    val state = init.value._1.map{
        case (name, variable) =>
            variable match {
              case SysCall(parameterTypes, body, _type) => (name, TFunctionCall(parameterTypes,_type))
              case _ => throw new TypeCheckException("A variable of type other than syscall in syscall collection. Bad, bad programmer!", "")
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

  def checkFunctionCall(name : String, parameters : Seq[Expression], loc : Int) : State[HashMap[String,  Type],Type]  = {
    def getFunction() : State[HashMap[String,  Type], Tuple3[Seq[Expression], Type, List[Type]]] = {
      for {
        functionType <- Base.getVariable(name)
      }
        yield {
          functionType match {
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
      calleeParamTypes = function._3
      returnType = function._2
      callParamTypes <- parameters.toList.traverse(checkExpression)
    }
    yield {
      if (callParamTypes != calleeParamTypes)
          throw new TypeCheckException(s"In call to function: '$name': parameters were of types: ${toString(callParamTypes)}, but function expected ${toString(calleeParamTypes)}", getLineNo(loc))

      returnType
    }

  }

  def checkStatement(stat : Statement) : State[HashMap[String,  Type],Unit]  = {
    stat match {
      case parser.VarAssignmentAndDeclaration(varname, expression, loc) =>
        for {
          t <- checkExpression(expression)
          _ <- Base.updateState(varname, t)
        }
        yield ()
      case VarAssignment(varname, expression, loc) =>
        for {
          exprType <- checkExpression(expression)
          varTypeOpt <- Base.getVariable(varname)
        }yield{
          varTypeOpt match{
            case Some(varType) =>
              if (exprType != varType)
                throw new TypeCheckException(s"Variable assigned to wrong type. Variable $varname is of type: ${toString(varType)} but was assigned a value of type ${toString(exprType)}", getLineNo(loc))
            case None =>
              throw new TypeCheckException(s"Tried to assign a value to an undeclared variable '$varname'. Maybe you forgot to declare the variable using the 'var' keyword?", getLineNo(loc))
          }

        }

      case parser.SubroutineCall(fcall, loc) =>
        checkFunctionCall(fcall.name, fcall.parameters, loc).flatMap(x => State(s => (s, ())))
      case parser.IfThenElse(_condition, _then, _else, loc) =>
        (for {
          condType <- checkExpression(_condition)
        }
        yield {
          if (!condType.isInstanceOf[TBool])
            throw new TypeCheckException("Expression in if was not boolean. ", getLineNo(loc))
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
            case None => throw new TypeCheckException("Variable " + id + " not defined when referenced. ", "")
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

      case parser.FunctionCall(name, parameters, loc) =>
        checkFunctionCall(name, parameters, loc)
    }
  }
}
