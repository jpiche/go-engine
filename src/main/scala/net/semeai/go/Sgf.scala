package net.semeai.go

import java.io.Reader
import scala.io.Source
import scala.util.parsing.combinator._

import scalaz._, Scalaz._

/*
 * Smart Game Format EBNF Definition
 *
 *   Collection = GameTree { GameTree }
 *   GameTree   = "(" Sequence { GameTree } ")"
 *   Sequence   = Node { Node }
 *   Node       = ";" { Property }
 *   Property   = PropIdent PropValue { PropValue }
 *   PropIdent  = UcLetter { UcLetter }
 *   PropValue  = "[" CValueType "]"
 *   CValueType = (ValueType | Compose)
 *   ValueType  = (None | Number | Real | Double | Color | SimpleText |
 *                 Text | Point  | Move | Stone)
 */

trait SgfParser extends JavaTokenParsers {

  // \uFEFF is the UTF-8 BOM. Really?! An SGF file seriously had a BOM at
  // the start.
  lazy val gameTree = opt("\uFEFF") ~> complexSeq

  lazy val complexSeq: Parser[TreeLoc[SgfNode]] =
    "(" ~> sequence ~ rep(simpleSeq | complexSeq) <~ ")" ^^ {
      case s ~ Nil => s
      case s ~ t => t.foldLeft(s) { (tree, node) =>
        val newTree = tree insertDownLast node.root.tree
        newTree.parent.get
      }
    }

  lazy val simpleSeq: Parser[TreeLoc[SgfNode]] = "(" ~> sequence <~ ")"

  lazy val sequence: Parser[TreeLoc[SgfNode]] = rep1(node) ^^ {
    case x :: Nil => x.leaf.loc
    case x :: nodelist => nodelist.foldLeft(x.leaf.loc) { (t, n) =>
      t insertDownLast n.leaf
    }
  }

  lazy val node: Parser[SgfNode] = ";" ~> rep(move | size | komi | prop) ^^ {
    x => SgfNode(x)
  }

  lazy val size: Parser[SgfProp] = "SZ" ~> "[" ~> """\d+""" <~ "]" ^^ {
    x => SzProp(x.toInt)
  }

  lazy val komi: Parser[SgfProp] = "KM" ~> "[" ~> decimalNumber <~ "]" ^^ {
    x => KomiProp(BigDecimal(x))
  }

  lazy val move: Parser[SgfProp] = ("B" | "W") ~ ("[" ~> opt("""[a-z][a-z]""".r) <~ "]") ^^ {
    case "B" ~ None => PassProp(BLACK)
    case "B" ~ Some(x) => BlackMove(x.charAt(0).toInt - 97, x.charAt(1).toInt - 97)
    case "W" ~ None => PassProp(WHITE)
    case "W" ~ Some(x) => WhiteMove(x.charAt(0).toInt - 97, x.charAt(1).toInt - 97)
  }

  lazy val prop: Parser[SgfProp] = """[A-Z]{1,2}""".r ~ cvalue ~ rep(cvalue) ^^ {
    case "C" ~ x ~ Nil => CommentProp(x)
    case "RU" ~ x ~ Nil => RulesProp(x)
    case "PW" ~ x ~ Nil => Player(WHITE, x)
    case "PB" ~ x ~ Nil => Player(BLACK, x)
    case i ~ x ~ y => ComplexProp(i, x :: y)
  }

  // ?> specifies an atomic group which throws away previous group matches
  // thus preventing catastrophic backtracking which typically presents itself
  // as a java.lang.StackOverflowError
  val cvalue = "[" ~> """(?>\\\]|[^\]])*""".r <~ "]" ^^ {
    _.replaceAll("""\]""", "]")
  }
}

trait SgfGame {

  // TODO: map sgf tree into game tree
  def sgfGame(sgf: TreeLoc[SgfNode]): Option[Game] = {
    None
  }
}

object SGF extends SgfParser {

  // simple string input works for most cases, but for large
  // inputs we need to use a reader
  def parse(reader: Reader): ParseResult[TreeLoc[SgfNode]] = parseAll(gameTree, reader)
  def parse(input: String): ParseResult[TreeLoc[SgfNode]] = parseAll(gameTree, input)
}
