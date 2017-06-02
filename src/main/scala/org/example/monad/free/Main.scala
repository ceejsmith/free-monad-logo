package org.example.monad.free

object Main {
  val program = {
    import Lifting._
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
