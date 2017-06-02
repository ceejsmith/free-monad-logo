package org.example.monad.free

import cats.{Id, ~>}

object InterpreterId extends (Instruction ~> Id) {
  import Computations._
  override def apply[A](fa: Instruction[A]): Id[A] = fa match {
    case Forward(p, length) => forward(p, length)
    case Backward(p, length) => backward(p, length)
    case RotateLeft(p, degree) => left(p, degree)
    case RotateRight(p, degree) => right(p, degree)
    case ShowPosition(p) => println(s"showing position $p")
  }
}