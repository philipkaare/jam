package typecheck

import cats.Functor
import cats.data.State
import exceptions.TypeCheckException
import interpreter.{Base, SysCall, SysCalls, Variable}
import parser._
import cats.implicits._
import cats.data._
import cats.syntax.traverse
import interpreter.JamInterpreter.StatefulResult
import typecheck.Typechecker.{checkExpression, checkStatement, checkStatements}

import scala.collection.immutable.HashMap

object Typechecker {

  var getLineNo : Int => String = s => ""


  def check(p :Program): Unit = {
    val s = (for {
      _ <- SysCalls.initializeSyscalls()
      state <- Base.getState()
    } yield(state.toList.traverse(x =>
      x._2 match {
        case SysCall(parameterTypes, _, _type) => Right((x._1, TFunctionDecl(parameterTypes, _type)))
        case _ => Left("A variable of type other than syscall in syscall collection. Bad, bad programmer!")
      }))).run(HashMap()).value

    val result = s.flatMap(v => Some(v.flatMap(v1 => Right(v1._2.flatMap(state =>
      checkStatements(p.statements.toList, state.toMap)))).flatten))

    result match {
      case Some(v) => v match {
        case Left(msg) => println(msg)
        case _ => ()
      }
      case None => ()
    }
  }

  def checkStatements(statements : Seq[Statement], state : Map[String,  Type]): Either[String, Type] = {

    val res = (for {
      s <- statements.toList.traverse(checkStatement)
    } yield {
      s.last
    }).run(state).value

    res match {
      case Some(either) => either match {
        case Left(value) =>Left(value)
        case Right((_, value)) =>Right(value)
      }
      case _ =>Left("Unknown error when checking statements. TODO")
    }
  }

  def checkFunctionCall(name : String, parameters : Seq[Expression], state : Map[String,  Type], loc : Int) : Either[String, Type]  = {
    def getFunction() : Either[String, Tuple3[Seq[Expression], Type, Seq[Type]]] = {
        val functionType = state.get(name)
        functionType match {
          case Some(_type) => _type match {
            case TFunctionDecl(calleeParameters, returnType) =>
              Right((parameters, returnType, calleeParameters))
          }
          case None => Left("Call to undeclared function " + name + getLineNo(loc))
        }
      }
    def verifyParameters(callParamTypesEither : Either[String, Seq[Type]], calleeParamTypes: Seq[Type]) : Either[String, Unit] = {
      callParamTypesEither >>=
        (callParamTypes => {
          if (callParamTypes != calleeParamTypes)
            Left(s"In call to function: '$name': parameters were of types: ${Types.toString(callParamTypes)}, but function expected ${Types.toString(calleeParamTypes)}" + getLineNo(loc))
          else
            Right(())})
    }
    for {
      function <- getFunction()
      params = function._1
      calleeParamTypes = function._3
      returnType = function._2
      callParamTypes = params.toList.traverse(p => checkExpression(p, state))
      _ <- verifyParameters(callParamTypes, calleeParamTypes)
    } yield returnType
  }

  def toStatefulResult(res : State[Map[String,  Type], Type]) : Base.StatefulResult[Type, Type] =
    StateT(s => EitherT.right(Some(res.run(s).value)))

  def checkParamShadowsVariable(param_exists : List[Boolean], parameters :Seq[TypeBinding], loc : Int) : Base.StatefulResult[Type, Unit] = {
    val res : Either[String, Unit] = param_exists.zip(parameters.map{ case TypeBinding(varname, _)=>varname }).foldMap({
      case (exists, varname) => {
        val msg : String = s"Parameter $varname shadows a variable in the outer scope. Please rename it. " + getLineNo(loc)
        if (exists)
          Left(msg)
        else Right()
      }
    })
    StateT.liftF(EitherT.fromEither(res))
  }

  def checkFnameShadowsOuterVariable(exists : Boolean, loc : Int, name:String) : Base.StatefulResult[Type, Unit] = {
    val msg = s"Function $name shadows a variable in the outer scope. Please rename it. " + getLineNo(loc)
    if (exists)
      StateT.liftF(EitherT.leftT(msg))
    else
      StateT.liftF(EitherT.rightT(()))
  }

  def checkReturnType(parameters : Seq[TypeBinding], body : Seq[Statement], returnType : Type, loc : Int ) : Base.StatefulResult[Type, Unit]  = {
    StateT(s => {
      val params = parameters.map { case TypeBinding(varname, t) => (varname, t) }.toMap

      val result = checkStatements(body, s ++ params).flatMap(actualReturnType =>
      {
        if (returnType != actualReturnType)
        {
          val msg =s"Returning variable of type $actualReturnType in function declaring type $returnType" + getLineNo(loc)
          Left(msg)
        }
        else
          Right((s, ()))
      })
      EitherT.fromEither(result)
    })
  }

  def checkStatement(stat : Statement) : Base.StatefulResult[Type, Type]  = {
    stat match {
      case FunctionDeclaration(name, parameters, body,returnType:Type, loc) =>
        val decl : Type = TFunctionDecl(parameters.map(b => b._type), returnType)
        for{
          exists <- Base.exists(name)
          param_exists <- parameters.toList.traverse { case TypeBinding(varname, _) => Base.exists[Type](varname) }
          _ <- Base.updateState(name, decl)
          _ <- checkParamShadowsVariable(param_exists, parameters, loc)
          _ <- checkFnameShadowsOuterVariable(exists, loc, name)
          _ <- checkReturnType(parameters, body, returnType, loc)
        }
        yield TUnit()
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
          varType <- Base.getVariable[Type](varname)
        }
          yield {
            if (exprType != varType)
              throw new TypeCheckException(s"Variable assigned to wrong type. Variable $varname is of type: " +
                s"${Types.toString(varType)} but was assigned a value of type ${Types.toString(exprType)}", getLineNo(loc))
          TUnit()
        }
      case Return(varname, loc) =>
        //if (!isInBody)
        //  throw new TypeCheckException("Return outside function body. You can only return when inside a function. ", getLineNo(loc))
        //else
        Base.getVariable[Type](varname)
      case parser.SubroutineCall(fcall, loc) =>
        StateT(s =>{
          EitherT.fromEither(checkFunctionCall(fcall.name, fcall.parameters,s, loc)
            .flatMap(_ => Right((s, TUnit()))))
        })
      case parser.WhileLoop(_condition, _body, loc) =>
        StateT(s =>{
          EitherT.fromEither(checkExpression(_condition, s).flatMap(t => {
            if (!t.isInstanceOf[TBool])
              Left("Expression in while was not boolean. " + getLineNo(loc))
            else
            {
              checkStatements(_body.toList,s) >>= (t => Right((s, t)))
            }}))
        })
      case parser.IfThenElse(_condition, _then, _else, loc) =>
          StateT(s =>{
            EitherT.fromEither(checkExpression(_condition, s).flatMap(t => {
              if (!t.isInstanceOf[TBool])
                Left("Expression in if was not boolean. " + getLineNo(loc))
              else
                checkStatements(_then.toList,s) >>=
                (_ => checkStatements(_else.toList,s)) >>= (t => Right((s, t)))
              }))
          })
    }
  }

  def checkExpression(expr : Expression) : Base.StatefulResult[Type, Type] =
    StateT((s : Map[String, Type]) => EitherT.fromEither((checkExpression(expr, s).flatMap(t => Right((s,t))))))

  def checkExpression(expr : Expression, state: Map[String,  Type]) : Either[String, Type] = {
    expr match {
      case parser.ExprValue(value, loc) => value match {
        case PInt(_) => Right(TInt())
        case PString(_) =>  Right(TString())
        case PFloat(_) =>  Right(TFloat())
        case PBool(_)=> Right(TBool())
      }
      case parser.Identifier(id,loc) =>
        state.get(id) match {
          case Some( _type) => Right(_type)
          case None => Left("Variable " + id + " not defined when referenced. " + getLineNo(loc))
        }
      case parser.Expr(op, l, r, loc) =>
        {
          val leftType = checkExpression(l, state)
          val rightType = checkExpression(r, state)
          if (leftType != rightType)
            Left(s"Type mismatch in expression: $leftType not equal to $rightType" + getLineNo(loc))
          else
            op match {
              case _ : ArithmeticOperator => leftType
              case _ : BooleanOperator => //todo: Does it make sense to compare strings with < / > ?
                Right(TBool())
            }
        }
      case parser.FunctionCall(name, parameters, loc) =>
        checkFunctionCall(name, parameters, state, loc)
    }
  }
}
