package net.semeai.go

case class Board(
  size: Int,
  points: Map[Point,Color] = Map()
) {
  lazy val draw = Board.draw(this)

  def +(move: Move): Board = move match {
    case Pass(_, _) => this
    case p@Position(_, _, _) => {
      val (m, caps) = processCaptures(p)
      Board(size, m)
    }
  }

  def +(node: GameNode): Board = node match {
    case RootNode(_,_,_,_,_) => this
    case MoveNode(move,_) => this + move
  }

  def processCaptures(move: Position): (Map[Point, Color], Int) = {
    val friends = move.point.friends(size)
    val groups: List[List[Point]] = friends map { p =>
      points.get(p) match {
        case Some(color: Color) if color == move.color.flip => Some(findGroup(p, color))
        case _ => None
      }
    } filter { _.isDefined } map { _.get }

    (points + (move.point -> move.color), 0)
  }

  def findGroup(start: Point, color: Color): List[Point] = {
    @annotation.tailrec
    def loop(p: List[Point], color: Color, group: List[Point]): List[Point] = {
      p match {
        case Nil => group
        case x :: xs =>
          if (group contains x)
            group
          else
            points.get(x) match {
              case Some(c: Color) if c == color =>
                loop(x.friends(size), color, x :: group)
              case Some(_) =>
                group
              case None =>
                loop(xs, color, group)
            }
      }
    }
    loop(start :: Nil, color, Nil)
  }

  //def countLibs(group: List[Point]): Int

}

trait BoardFunctions {

  def draw(board: Board): String = {
    val size = board.size - 1
    val s = for {
      x <- Range.inclusive(size, 0, -1)
      y <- 0 to size
    } yield {
      val point = board.points.get((y, x)) match {
        case Some(Black) => "•"
        case Some(White) => "o"
        case None => "·"
      }
      if (y == size)
        point + "\n"
      else
        point
    }
    s.mkString
  }

  def empty(size: Int): Board = Board(size)
}

object Board extends BoardFunctions

