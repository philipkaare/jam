package interpreter

import scala.collection.immutable.HashMap
import scala.collection.mutable

object SysCalls {

  def initializeSyscalls(state : HashMap[String,Variable]): HashMap[String, Variable] = {
    state +
      ("print" -> SysCall(List("string"), params => {
      System.out.println(params.head)
      None
    }, "Unit")) +
      ("intToString" -> SysCall(List("int"), params => {
        Some(params.head.toString)
      }, "string"))
  }

}
