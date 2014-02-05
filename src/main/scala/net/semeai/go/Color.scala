package net.semeai.go

sealed trait Color {
  val flip: Color
}

object BLACK extends Color {
  val flip = WHITE
}

object WHITE extends Color {
  val flip = BLACK
}
