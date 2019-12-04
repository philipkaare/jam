package interpreter

import exceptions.RuntimeTypeException
import typecheck.{TFloat, TInt, TString, TUnit}

import scala.collection.immutable.HashMap
import scala.collection.mutable

object SysCalls {

  def initializeSyscalls(state : HashMap[String,Variable]): HashMap[String, Variable] = {
    state +
      ("print" -> SysCall(List(TString()), params => {
        params.head match {
          case StringVar(s) => System.out.println(s)
          case _ => throw new RuntimeTypeException("print called with a non-string parameter!")
        }
        None
      }, TUnit())) +
      ("intToString" -> SysCall(List(TInt()), params => {
        params.head match {
          case IntVar(i) =>
            Some(StringVar(i.toString))
          case _ => throw new RuntimeTypeException("intToString called with a non-int parameter!")
        }

      }, TString())) +
      ("floatToString" -> SysCall(List(TFloat()), params => {
      params.head match {
        case FloatVar(i) =>
          Some(StringVar(i.toString))
        case _ => throw new RuntimeTypeException("floatToString called with a non-int parameter!")
      }

    }, TString()))
  }

}
