package org.example.monad.free

object Main {
  private def createProgram(start: Position) = {
    import Lifting._
    for {
      p1 <- forward(start, 10.0)
      p2 <- left(p1, Degree(45))
      p3 <- forward(p2, 14.14213562373095)
      _ <- showPosition(p3)
    } yield ()
  }

  def main(args: Array[String]): Unit = {
    val startPosition = Position(0.0, 0.0, Degree(0))
    println("Creating program ...")
    val program = createProgram(startPosition)
    println()
    println("Executing program ...")
    val _ = program.foldMap(InterpreterId)
  }
}
