package net.semeai.go

import scalaz._, Scalaz._

class Game(
  val size: Int,
  zipper: TreeLoc[GameNode] = {
    val node: GameNode = RootNode()
    node.leaf.loc
  }
) {
  def +(move: Move): Game = new Game(
    size,
    zipper insertDownFirst {
      val node: GameNode = MoveNode(move)
      node
    }.leaf
  )
  def +(p: Point): Game = this + Position(next, p)

  lazy val current: GameNode = zipper.tree.rootLabel
  lazy val next: Color = current match {
    case _: RootNode => BLACK
    case m: MoveNode => m.move.color.flip
  }

  lazy val board: Board = {
    @annotation.tailrec
    def build(board: Board, tree: TreeLoc[GameNode]): Board = {
      val b = board + tree.tree.rootLabel
      tree.firstChild match {
        case None => b
        case Some(t) => build(b + t.tree.rootLabel, t)
      }
    }
    build(Board.empty(size), zipper.root)
  }
}

object Game {
  def apply(size: Int) = new Game(size)
}
