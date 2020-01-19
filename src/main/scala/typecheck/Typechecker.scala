package typecheck

import cats.Functor
import cats.data.State
import exceptions.TypeCheckException
import interpreter.{Base, SysCall, SysCalls, Variable}
import parser._
import cats.implicits._
import cats.data._
import cats.syntax.traverse
import typecheck.Typechecker.{checkExpression, checkStatement, checkStatements}

import scala.collection.immutable.HashMap

object Typechecker {

  var getLineNo : Int => String = s => ""


  def check(p :Program): Unit = {
    val init = SysCalls.initializeSyscalls().run(HashMap())
    val state : HashMap[String, Type] = init.value._1.map{
        case (name, variable) =>
            variable match {
              case SysCall(parameterTypes, _, _type) => (name, TFunctionDecl(parameterTypes,_type))
              case _ => throw new TypeCheckException("A variable of type other than syscall in syscall collection. Bad, bad programmer!", "")
            }
      }
    checkStatements(p.statements.toList,state)
  }

  def checkStatements(statements : Seq[Statement], state : HashMap[String,  Type]): Type =

    (for {
        s <- statements.toList.traverse( checkStatement)
      } yield {
        s.last
      }).run(state).value._2


  def checkFunctionCall(name : String, parameters : Seq[Expression], loc : Int) : State[HashMap[String,  Type],Type]  = {
    def getFunction() : State[HashMap[String,  Type], Tuple3[Seq[Expression], Type, Seq[Type]]] = {
      for {
        functionType <- Base.getVariable(name)
      }
        yield {
          functionType match {
            case Some(_type) => _type match {
              case TFunctionDecl(calleeParameters, returnType) =>
                (parameters, returnType, calleeParameters)
            }
            case None => throw new TypeCheckException("Call to undeclared function " + name, getLineNo(loc))
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
          throw new TypeCheckException(s"In call to function: '$name': parameters were of types: ${Types.toString(callParamTypes)}, but function expected ${Types.toString(calleeParamTypes)}", getLineNo(loc))

      returnType
    }

  }

  def checkStatement(stat : Statement) : State[HashMap[String,  Type],Type]  = {
    stat match {
      case FunctionDeclaration(name, parameters, body,returnType:Type, loc) =>
        val decl : Type = TFunctionDecl(parameters.map(b => b._type), returnType)
        (for{
          exists <- Base.exists(name)
          param_exists <- parameters.toList.traverse { case TypeBinding(varname, _) => Base.exists[Type](varname) }
          _ <- Base.updateState(name, decl)
        }
        yield {
          param_exists.zip(parameters.map{ case TypeBinding(varname, _)=>varname }).foreach({
            case (exists, varname) => if (exists)
              throw new TypeCheckException(s"Parameter $varname shadows a variable in the outer scope. Please rename it. ", getLineNo(loc))
          })
          if (exists)
            throw new TypeCheckException(s"Function $name shadows a variable in the outer scope. Please rename it. ", getLineNo(loc))
          parameters
        }).flatMap(parameters =>
          State(s => {
            val params = (parameters.map { case TypeBinding(varname, t) =>  (varname, t)}).toMap

            val actualReturnType = checkStatements(body, s ++ params)
            if (returnType != actualReturnType)
              throw new TypeCheckException(s"Returning variable of type $actualReturnType in function declaring type $returnType", getLineNo(loc))
            (s, TUnit())
          }))
      case parser.VarAssignmentAndDeclaration(varname, expression, loc) =>
        for {
          t <- checkExpression(expression)
          exists <- Base.exists(varname)
          _ <- Base.updateState(varname, t)
        }
        yield {
          if (exists)
            throw new TypeCheckException(s"Variable $varname already defined in scope. ",getLineNo(loc))
          else
            TUnit()
        }
      case VarAssignment(varname, expression, loc) =>
        for {
          exprType <- checkExpression(expression)
          varTypeOpt <- Base.getVariable(varname)
        }yield{
          varTypeOpt match{
            case Some(varType) =>
              if (exprType != varType)
                throw new TypeCheckException(s"Variable assigned to wrong type. Variable $varname is of type: ${Types.toString(varType)} but was assigned a value of type ${Types.toString(exprType)}", getLineNo(loc))
            case None =>
              throw new TypeCheckException(s"Tried to assign a value to an undeclared variable '$varname'. Maybe you forgot to declare the variable using the 'var' keyword?", getLineNo(loc))
          }
          TUnit()
        }
      case Return(varname, loc) =>
        //if (!isInBody)
        //  throw new TypeCheckException("Return outside function body. You can only return when inside a function. ", getLineNo(loc))
        //else
        {
          for {
            t <- Base.getVariable[Type](varname)
          } yield {
            t match {
              case Some(_type) => _type
            }
          }
        }
      case parser.SubroutineCall(fcall, loc) =>
        checkFunctionCall(fcall.name, fcall.parameters, loc).flatMap(_ => State(s => (s, TUnit())))
      case parser.IfThenElse(_condition, _then, _else, loc) =>
        (for {
          condType <- checkExpression(_condition)
        }
        yield {
          if (!condType.isInstanceOf[TBool])
            throw new TypeCheckException("Expression in if was not boolean. ", getLineNo(loc))
          ()
        }).flatMap(_ => {
          State(s =>{
            checkStatements(_then.toList,s)
            checkStatements(_else.toList,s)
            (s, TUnit())
          })
        })

    }
  }

  def checkExpression(expr : Expression) : State[HashMap[String,  Type],Type] = {
    expr match {
      case parser.ExprValue(value, loc) => value match {
        case PInt(_) => State(s => (s,TInt()))
        case PString(_) =>  State(s => (s,TString()))
        case PFloat(_) =>  State(s => (s,TFloat()))
        case PBool(_)=> State(s => (s, TBool()))
      }
      case parser.Identifier(id,loc) =>
        for {
          v <- Base.getVariable(id)
        } yield{
          v match {
            case Some( _type) => _type
            case None => throw new TypeCheckException("Variable " + id + " not defined when referenced. ", getLineNo(loc))
        }
      }
      case parser.Expr(op, l, r, loc) =>
        for {
          leftType <- checkExpression(l)
          rightType <- checkExpression(r)

        }
        yield {
          if (leftType != rightType)
            throw new TypeCheckException(s"Type mismatch in expression: $leftType not equal to $rightType", getLineNo(loc))
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
