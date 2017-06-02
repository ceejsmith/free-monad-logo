package org.example.monad.free

import cats.free.Free
import cats.{Id, ~>}

case class Position(x: Double, y: Double, heading: Degree)
case class Degree(private val d: Int) {
  val value = d % 360
  val rads = Math.PI * value / 180.0
}

object Logo {
  sealed trait Instruction[A]
  case class Forward(position: Position, length: Int) extends Instruction[Position]
  case class Backward(position: Position, length: Int) extends Instruction[Position]
  case class RotateLeft(position: Position, degree: Degree) extends Instruction[Position]
  case class RotateRight(position: Position, degree: Degree) extends Instruction[Position]
  case class ShowPosition(position: Position) extends Instruction[Unit]

  def forward(pos: Position, l: Int): Free[Instruction, Position] = Free.liftF(Forward(pos, l))
  def backward(pos: Position, l: Int): Free[Instruction, Position] = Free.liftF(Backward(pos, l))
  def left(pos: Position, degree: Degree): Free[Instruction, Position] = Free.liftF(RotateLeft(pos, degree))
  def right(pos: Position, degree: Degree): Free[Instruction, Position] = Free.liftF(RotateRight(pos, degree))
  def showPosition(pos: Position): Free[Instruction, Unit] = Free.liftF(ShowPosition(pos))

  val program: (Position => Free[Instruction, Position]) = {
    start: Position =>
      for {
        p1 <- forward(start, 10)
        p2 <- right(p1, Degree(90))
        p3 <- forward(p2, 10)
      } yield p3
  }

  def main(args: Array[String]): Unit = {
    val startPosition = Position(0.0, 0.0, Degree(0))

    println(program(startPosition).foldMap(InterpreterId))
  }
}

import Logo._

object InterpreterId extends (Instruction ~> Id) {
  import Computations._
  override def apply[A](fa: Logo.Instruction[A]): Id[A] = fa match {
    case Forward(p, length) => forward(p, length)
    case Backward(p, length) => backward(p, length)
    case RotateLeft(p, degree) => left(p, degree)
    case RotateRight(p, degree) => right(p, degree)
    case ShowPosition(p) => println(s"showing position $p")
  }
}