package interpreter

import cats.data.State

import scala.collection.immutable.HashMap

object Base {
  def updateState[T](key : String, value: T) : State[HashMap[String, T], Unit] = {
    State(s => (s + (key -> value), ()))
  }

  def getVariable[T](key : String) : State[HashMap[String, T], Option[T]] = {
    State(s => (s, s.get(key)))
  }

  def getLineNo(input : String) = (pos : Int ) => {
    val (untilPos,_) = input.splitAt(pos)
    val lineno = untilPos.count(c => c == '\n') + 1
    val linepos = pos-untilPos.lastIndexOf('\n') - 1
    s"At line: $lineno, position: $linepos"
  }

}
