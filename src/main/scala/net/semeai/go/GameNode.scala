package net.semeai.go

sealed trait GameNode

final case class MoveNode(
  move: Move,
  comments: List[String] = Nil
) extends GameNode

final case class RootNode(
  black: String = "Black",
  white: String = "White",
  rules: String = "Japanese",
  komi: BigDecimal = 0,
  size: Int = 19
) extends GameNode
