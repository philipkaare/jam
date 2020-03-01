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
              Some(StringVar(scala.io.StdIn.readLine()))
            }, TString()))
      _ <- Base.updateState[Variable]("print", SysCall(List(TString()), params => {
              params.head match {
                case StringVar(s) => System.out.println(s)
                case _ => throw new RuntimeTypeException("print called with a non-string parameter!")
              }
              Some(UnitVar())
            }, TUnit()))
      _ <- Base.updateState[Variable]("intToString", SysCall(List(TInt()), params => {
            params.head match {
              case IntVar(i) =>
                Some(StringVar(i.toString))
              case _ => throw new RuntimeTypeException("intToString called with a non-int parameter!")
            }

          }, TString()))
      _ <- Base.updateState[Variable]("stringToInt", SysCall(List(TString()), params => {
              params.head match {
                case StringVar(value) =>
                  Some(IntVar(Integer.parseInt(value)))
                case _ => throw new RuntimeTypeException("stringToInt called with a non-string parameter!")
              }
            }, TInt()))
      _ <- Base.updateState[Variable]("floatToString", SysCall(List(TFloat()), params => {
              params.head match {
                case FloatVar(i) =>
                  Some(StringVar(i.toString))
                case _ => throw new RuntimeTypeException("floatToString called with a non-int parameter!")
              }

            }, TString()))
    } yield ()

  }
}