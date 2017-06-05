package org.example.monad.free

import Free.{Id, ~>}
import Computations._

object InterpreterId extends (Instruction ~> Id) {
  override def apply[A](fa: Instruction[A]): Id[A] = fa match {
    case Forward(p, length) => {
      println(s"Moving forward a distance of $length from (${p.x}, ${p.y})")
      forward(p, length)
    }
    case Backward(p, length) => {
      println(s"Moving backward a distance of $length from (${p.x}, ${p.y})")
      backward(p, length)
    }
    case RotateLeft(p, degree) => {
      println(s"Rotating left through ${degree.value} degrees on point (${p.x}, ${p.y})")
      left(p, degree)
    }
    case RotateRight(p, degree) => {
      println(s"Rotating right through ${degree.value} degrees on point (${p.x}, ${p.y})")
      right(p, degree)
    }
    case ShowPosition(p) => println(s"Now at (${p.x}, ${p.y}) facing ${p.heading.value} degrees")
  }
}