package net.semeai.go

import org.specs2.mutable._
import scala.io.Source


class SgfSpec extends Specification {

  "An SGF Parser" should {
    "parse a simple game" in {
      val f = Source.fromURL(getClass.getResource("/01.sgf")).reader()
      SGF.parse(f).successful must beTrue
    }

    "parse escaped comments" in {
      val f = Source.fromURL(getClass.getResource("/02.sgf")).reader()
      SGF.parse(f).successful must beTrue
    }

    "parse a large, complex file" in {
      val f = Source.fromURL(getClass.getResource("/kogo.sgf")).reader()
      SGF.parse(f).successful must beTrue
    }
  }
}
