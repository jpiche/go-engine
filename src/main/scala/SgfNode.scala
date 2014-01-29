package net.semeai.go

import scalaz._, Scalaz._


sealed trait SgfProp
sealed trait MoveProp extends SgfProp
final case class PassProp(color: Color) extends MoveProp
final case class CommentProp(comment: String) extends SgfProp
final case class SzProp(size: Int) extends SgfProp
final case class KomiProp(komi: BigDecimal) extends SgfProp
final case class RulesProp(rules: String) extends SgfProp
final case class Player(color: Color, name: String) extends SgfProp
final case class ComplexProp(key: String, props: List[String]) extends SgfProp

sealed trait ColorMoveProp extends MoveProp {
  val x: Int
  val y: Int

  def toPoint(size: Int) = Point(size - 1 - x, y)
}

final case class BlackMove(x: Int, y: Int) extends MoveProp
final case class WhiteMove(x: Int, y: Int) extends MoveProp

class SgfNode(
  val props: List[SgfProp]
) {
  lazy val size: Option[Int] = props.reverse collectFirst {
    case SzProp(s) => s
  }

  lazy val move: Option[MoveProp] = props.reverse collectFirst {
    case b@BlackMove(_, _) => b
    case w@WhiteMove(_, _) => w
    case p@PassProp(_) => p
  }
}

object SgfNode {
  def apply(p: List[SgfProp]) = new SgfNode(p)

  implicit def nodeShow = new Show[SgfNode] {
    override def show(n: SgfNode) = n.props match {
      case x :: Nil => x.toString
      case x => x.toString
    }
  }
}

