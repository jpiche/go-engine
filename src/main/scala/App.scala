package net.semeai.go

import java.io.FileReader

import scala.util.parsing.combinator._
import scala.io.Source

object app extends App {
  val f = Source.fromURL(getClass.getResource("/kogo.sgf")).reader
  val g = SGF.parse(f)
  println(g)
}

