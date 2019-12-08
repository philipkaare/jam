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

}
