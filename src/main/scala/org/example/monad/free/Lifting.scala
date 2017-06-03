package org.example.monad.free

import org.example.monad.free.Free._

object Lifting {
  def forward(pos: Position, l: Int): FreeB[Instruction, Position] = Free.liftF(Forward(pos, l))
  def backward(pos: Position, l: Int): FreeB[Instruction, Position] = Free.liftF(Backward(pos, l))
  def left(pos: Position, degree: Degree): FreeB[Instruction, Position] = Free.liftF(RotateLeft(pos, degree))
  def right(pos: Position, degree: Degree): FreeB[Instruction, Position] = Free.liftF(RotateRight(pos, degree))
  def showPosition(pos: Position): FreeB[Instruction, Unit] = Free.liftF(ShowPosition(pos))
}
