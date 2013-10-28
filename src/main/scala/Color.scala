package net.semeai.go

sealed trait Color {
  val flip: Color
}
final case object Black extends Color {
  final val flip = White
}
final case object White extends Color {
  final val flip = Black
}

