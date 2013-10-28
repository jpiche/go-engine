package net.semeai.go

import scalaz._
import Scalaz._

class Game(
  size: Int,
  zipper: TreeLoc[GameNode]
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
    case RootNode(_,_,_,_,_) => Black
    case MoveNode(move,_) => move.color.flip
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
  def apply(size: Int, node: GameNode) = new Game(size, node.leaf.loc)
  def apply(size: Int) = new Game(size, {
    val node: GameNode = RootNode(size = size)
    node
  }.leaf.loc)
  def apply(root: RootNode) = new Game(root.size, {
    val node: GameNode = root
    node
  }.leaf.loc)
}

