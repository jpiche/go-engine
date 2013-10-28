package net.semeai.go

import org.specs2.mutable._

class GameSpec extends Specification {
  "An empty game" should {
    "print an empty grid" in {

      Game(5).board.draw === (("·" * 5) + "\n") * 5
      Game(6).board.draw === (("·" * 6) + "\n") * 6
      Game(7).board.draw === (("·" * 7) + "\n") * 7
      Game(8).board.draw === (("·" * 8) + "\n") * 8
      Game(9).board.draw === (("·" * 9) + "\n") * 9
      Game(10).board.draw === (("·" * 10) + "\n") * 10
      Game(11).board.draw === (("·" * 11) + "\n") * 11
      Game(12).board.draw === (("·" * 12) + "\n") * 12
    }
  }

  "Simple moves" should {
    "print correct moves" in {
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

