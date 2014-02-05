package net.semeai.go


final class Board(
  val size: Int,
  val points: Map[Point,Color]
) {
  lazy val draw = Board.draw(this)

  def each(f: (Point, Option[Color]) => Any) = {
    val xsize = size - 1
    for {
      x <- Range.inclusive(xsize, 0, -1)
      y <- 0 to xsize
      p = Point(x, y)
      π = points.get(p)
    } yield f(p, π)
  }

  def +(move: Move): Board = move match {
    case pass: Pass => this
    case pos: Position =>
      val (m, caps) = processCaptures(pos)
      Board(size, m)
  }

  def +(node: GameNode): Board = node match {
    case root: RootNode => this
    case MoveNode(move,_) => this + move
  }

  def processCaptures(move: Position): (Map[Point, Color], Int) = {
    val friends = move.point.friends(size)

//    val groups: List[List[Point]] = friends map { p =>
//      points.get(p) match {
//        case Some(color: Color) if color == move.color.flip => Some(findGroup(p, color))
//        case _ => None
//      }
//    } filter { _.isDefined } map { _.get }

    val groups: List[List[Point]] = for {
      friend <- friends
      p = points get friend
      if p.isDefined
      color = p.get
      if color == move.color.flip
    } yield
      findGroup(friend, color)

    val caps = groups.filter(countLibs(_) == 0).flatten
    val map = caps.foldRight(points) { (p, m) =>
      m - p
    }

    (map + (move.point -> move.color), caps.size)
  }

  def findGroup(start: Point, color: Color): List[Point] = {
    @annotation.tailrec
    def loop(search: List[Point], group: List[Point]): List[Point] =
      search match {
        case Nil => group
        case x :: xs =>
          if (group contains x)
            loop(xs, group)
          else
            points.get(x) match {
              case Some(c: Color) if c == color =>
                loop(x.friends(size), x :: group)
              case _ =>
                loop(xs, group)
            }
      }
    loop(start :: Nil, Nil)
  }

  def countLibs(group: List[Point]): Int = {
    @annotation.tailrec
    def loop(p: List[Point], count: Int): Int = p match {
      case Nil => count
      case x :: xs =>
        val friends = x.friends(size)
        val libs = for {
          f <- friends
          p = points get f
          if p.isEmpty
        } yield f
        loop(xs, libs.distinct.size + count)
    }
    loop(group, 0)
  }
}

trait BoardFunctions {

  def draw(board: Board): String = {
    val size = board.size - 1
    val s = for {
      x <- Range.inclusive(size, 0, -1)
      y <- 0 to size
    } yield {
      val point = board.points.get((y, x)) match {
        case Some(BLACK) => "•"
        case Some(WHITE) => "o"
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

object Board extends BoardFunctions {
  def apply(size: Int): Board = new Board(size, Map())
  def apply(size: Int, map: Map[Point,Color]): Board = new Board(size, map)
}
