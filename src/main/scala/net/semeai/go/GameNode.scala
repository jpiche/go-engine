package net.semeai.go

sealed trait GameNode

final case class MoveNode(
  move: Move,
  comments: List[String] = Nil,
  blackCaps: Int = 0,
  whiteCaps: Int = 0
) extends GameNode

final case class RootNode(
  black: String = "Black",
  white: String = "White",
  rules: String = "Japanese",
  komi: BigDecimal = 0
) extends GameNode
