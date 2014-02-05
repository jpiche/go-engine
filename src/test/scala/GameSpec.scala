package net.semeai.go

import org.specs2.mutable._
import org.specs2.execute.Result

class GameSpec extends Specification {
  "An empty game" should {
    "print empty grids 1x1 to 52x52" in {
      Result.unit {
        (1 to 52) foreach { i =>
          Game(i).board.draw must_== (("·" * i) + "\n") * i
        }
      }
    }
  }

  "Simple manual moves" should {
    "print a correct board" in {
      val g = Game(5) + Point(0, 0) + Point(1,1) + Point(3,3) + Point(0, 4) + Point(0,1) + Point(0, 2)
      g.board.draw === Seq(
        "o····",
        "···•·",
        "o····",
        "•o···",
        "•····"
      ).mkString("", "\n", "\n")
    }
  }

  "Simple captures" should {
    "capture correctly in the corner" in {
      val g = Game(5) + Point(0, 0) + Point(1,1) + Point(3,3) + Point(0, 4) + Point(0,1) + Point(0, 2) + Point(1, 0) + Point(2, 0)
      g.board.draw === Seq(
        "o····",
        "···•·",
        "o····",
        "·o···",
        "··o··"
      ).mkString("", "\n", "\n")
    }

    "capture in the middle correctly" in {
      val g = Game(6) + Point(2, 1) + Point(3, 1) + Point(3,2) + Point(2, 2) + Point(3, 3) + Point(2,3) + Point(2,4) + Point(3, 4) + Point(4, 4) + Point(4, 3) + Point(0, 0) + Point(4, 2)
      g.board.draw === Seq(
        "······",
        "··•o•·",
        "··o·o·",
        "··o·o·",
        "··•o··",
        "•·····"
      ).mkString("", "\n", "\n")
    }
  }
}
