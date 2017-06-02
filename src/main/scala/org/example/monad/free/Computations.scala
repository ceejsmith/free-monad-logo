package org.example.monad.free

object Computations {
  def forward(p: Position, l: Int) = Position(p.x + l * Math.cos(p.heading.rads), p.y + l * Math.sin(p.heading.rads), p.heading)
  def backward(p: Position, l: Int) = Position(p.x - l * Math.cos(p.heading.rads), p.y - l * Math.sin(p.heading.rads), p.heading)
  def left(p: Position, degree: Degree) = p.copy(heading = Degree(p.heading.value + degree.value))
  def right(p: Position, degree: Degree) = p.copy(heading = Degree(p.heading.value - degree.value))
}
