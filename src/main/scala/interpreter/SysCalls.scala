package interpreter

import cats.Functor
import cats.data.State
import cats.implicits._
import exceptions.RuntimeTypeException
import interpreter.Base.StatefulResult
import interpreter.Base.Result
import typecheck.{TFloat, TInt, TString, TUnit}



import scala.collection.immutable.HashMap
import scala.collection.mutable

object SysCalls {

  def initializeSyscalls() : StatefulResult[Variable, Unit] = {
    for {
      _ <- Base.updateState[Variable]("read", SysCall(List(), params => {
              Right(StringVar(scala.io.StdIn.readLine()))
            }, TString()))
      _ <- Base.updateState[Variable]("print", SysCall(List(TString()), params => {
              params.head match {
                case StringVar(s) => {
                  System.out.println(s)
                  Right(UnitVar())
                }
                case _ => Left("print called with a non-string parameter!")
              }
            }, TUnit()))
      _ <- Base.updateState[Variable]("intToString", SysCall(List(TInt()), params => {
            params.head match {
              case IntVar(i) => Right(StringVar(i.toString))
              case _ => Left("intToString called with a non-int parameter!")
            }
          }, TString()))
      _ <- Base.updateState[Variable]("stringToInt", SysCall(List(TString()), params => {
              params.head match {
                case StringVar(value) => Right(IntVar(Integer.parseInt(value)))
                case _ => Left("stringToInt called with a non-string parameter!")
              }
            }, TInt()))
      _ <- Base.updateState[Variable]("floatToString", SysCall(List(TFloat()), params => {
              params.head match {
                case FloatVar(i) => Right(StringVar(i.toString))
                case _ => Left("floatToString called with a non-int parameter!")
              }}, TString()))
    } yield ()

  }
}