package net.semeai.go

import scalaz._
import Scalaz._
import org.joda.time.Instant

sealed trait Move {
  val color: Color
  val time: Option[Instant]
}

case class Position(
  color: Color,
  point: Point,
  time: Option[Instant] = Instant.now.some
) extends Move

case class Pass(
  color: Color,
  time: Option[Instant] = Instant.now.some
) extends Move

