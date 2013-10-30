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
}

