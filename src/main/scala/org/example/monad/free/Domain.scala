package org.example.monad.free

case class Position(x: Double, y: Double, heading: Degree)

case class Degree(private val d: Int) {
  val value = d % 360
  val rads = Math.PI * value / 180.0
}