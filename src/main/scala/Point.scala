package net.semeai.go

import scalaz._
import Scalaz._

final case class Point(
  x: Int,
  y: Int
) {
  def friends(limit: Int): List[Point] = Point.directions map {
    Point.moveBy(this, _, limit)
  } filter { _.isDefined } map { _.get }

  override def toString = "(%d,%d)" format (x, y)
}

trait PointFunctions {
  implicit def tupleToPoint(t: (Int, Int)): Point = Point(t._1, t._2)

  val UP = (0, 1)
  val DOWN = (0, -1)
  val RIGHT = (1, 0)
  val LEFT = (-1, 0)
  val directions = List(UP, DOWN, LEFT, RIGHT)

  def moveBy(a: Point, b: Point, limit: Int): Option[Point] = {
    val x = a.x + b.x
    val y = a.y + b.y
    if (x < 0
      || y < 0
      || x >= limit
      || y >= limit
    )
      None
    else
      Point(x, y).some
  }
}

object Point extends PointFunctions

