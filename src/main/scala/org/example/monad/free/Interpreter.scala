package org.example.monad.free

import cats.{Id, ~>}
import Computations._

object InterpreterId extends (Instruction ~> Id) {

  override def apply[A](fa: Instruction[A]): Id[A] = fa match {
    case Forward(p, length) => forward(p, length)
    case Backward(p, length) => backward(p, length)
    case RotateLeft(p, degree) => left(p, degree)
    case RotateRight(p, degree) => right(p, degree)
    case ShowPosition(p) => println(s"showing position $p")
  }
}

object InterpreterOpt extends (Instruction ~> Option) {
  private def nonNegative(p: Position) = if (p.x >= 0 && p.y >= 0) Some(p) else None

  def apply[A](fa: Instruction[A]): Option[A] = fa match {
    case Forward(p, length) => nonNegative(forward(p, length))
    case Backward(p, length) => nonNegative(backward(p, length))
    case RotateLeft(p, degree) => Some(left(p, degree))
    case RotateRight(p, degree) => Some(right(p, degree))
    case ShowPosition(p) => Some(println(s"showing position $p"))
  }
}