package interpreter

import typecheck.{TInt, TString, TUnit}

import scala.collection.immutable.HashMap
import scala.collection.mutable

object SysCalls {

  def initializeSyscalls(state : HashMap[String,Variable]): HashMap[String, Variable] = {
    state +
      ("print" -> SysCall(List(TString()), params => {
      System.out.println(params.head)
      None
    }, TUnit())) +
      ("intToString" -> SysCall(List(TInt()), params => {
        Some(StringVar(params.head.toString))
      }, TString()))
  }

}
