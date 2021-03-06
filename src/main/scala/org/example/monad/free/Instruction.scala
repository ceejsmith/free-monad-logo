package org.example.monad.free

sealed trait Instruction[A]
case class Forward(position: Position, length: Int) extends Instruction[Position]
case class Backward(position: Position, length: Int) extends Instruction[Position]
case class RotateLeft(position: Position, degree: Degree) extends Instruction[Position]
case class RotateRight(position: Position, degree: Degree) extends Instruction[Position]
case class ShowPosition(position: Position) extends Instruction[Unit]
