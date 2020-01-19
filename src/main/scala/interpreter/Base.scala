package interpreter

import cats.data.State

import scala.collection.immutable.HashMap
import cats.syntax.traverse
import cats.implicits._
import cats.data._

object Base {
  def setParameters[T](names : Seq[String], values : Seq[T]) : State[HashMap[String, T], Unit] = {

    for {
      _ <- names.zip(values).toList.traverse({case (name, value) => updateState(name, value)})
    }
    yield()

  }

  def updateState[T](key : String, value: T) : State[HashMap[String, T], Unit] = {
    State(s => (s + (key -> value), ()))
  }

  def getVariable[T](key : String) : State[HashMap[String, T], Option[T]] = {
    State(s => (s, s.get(key)))
  }

  def exists[T](key : String) : State[HashMap[String, T], Boolean] = {
    State(s => (s, s.get(key).isDefined))
  }

  def getLineNo(input : String) = (pos : Int ) => {
    val (untilPos,_) = input.splitAt(pos)
    val lineno = untilPos.count(c => c == '\n') + 1
    val linepos = pos-untilPos.lastIndexOf('\n') - 1
    s"At line: $lineno, position: $linepos"
  }

}
