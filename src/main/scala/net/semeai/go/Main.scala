package net.semeai.go

import java.io.FileReader


object Main extends App with MainT {
  processArgs(args.toList)
}

trait MainT {

  def processArgs(args: List[String]): Unit = {
    args match {
      case Nil =>
        println("No actions to perform")
      case "read" :: file :: nextArgs =>
        processFileArg(file)
      case "help" :: Nil =>
        printHelp()
      case _ =>
        println("Unable to understand arguments")
        printHelp()
    }
  }

  def processFileArg(file: String): Unit = {
    val fr = new FileReader(file)
    val game = SGF.parse(fr)
    println(game)
  }

  def printHelp(): Unit = {
    val help =
      """
        |GoEngine version X.X
        |Usage:
        |  ./go read FILE     Reads SGF file
        |  ./go help          Prints this help message
      """.stripMargin

    println(help)
  }
}