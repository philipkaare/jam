package interpreter

import cats.data.{State, _}

import scala.collection.immutable.HashMap
import cats.syntax.traverse
import cats.implicits._

object Base {
  type Result[A] = EitherT[Option, String, A]
  type InternalState[T] = Map[String, T]
  type StatefulResult[T,V] = StateT[Result, InternalState[T], V]

  def setParameters[T](names : Seq[String], values : Seq[T]) : StatefulResult[T, Unit] = {
    for {
      _ <- names.zip(values).toList.traverse({case (name, value) => updateState(name, value)})
    }
    yield()
  }

  def updateState[T](key : String, value: T) : StatefulResult[T, Unit] = {
    StateT(s => EitherT.pure((s + (key -> value), ())))
  }

  def getState[T]() : StatefulResult[T, Map[String, T]]= {
    StateT((s : Map[String, T]) => EitherT.pure((s, s)))
  }

  def getVariable[T](key : String) : StatefulResult[T, T] = {
    StateT((s : Map[String, T]) => s.get(key) match {
      case Some(v) => EitherT.rightT((s,v))
      case None => EitherT.leftT("Variable " + key + " not defined when referenced. ")
    })
  }

  def getLineNo(input : String) = (pos : Int ) => {
    val (untilPos,_) = input.splitAt(pos)
    val lineno = untilPos.count(c => c == '\n') + 1
    val linepos = pos-untilPos.lastIndexOf('\n') - 1
    s"At line: $lineno, position: $linepos"
  }

}
