package net.semeai.go

import java.io.Reader
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

trait SgfParser extends RegexParsers {

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

  lazy val node: Parser[SgfNode] = ";" ~> rep(move | prop) ^^ {
    x => SgfNode(x)
  }

  lazy val move: Parser[SgfProp] = ("B" | "W") ~ ("[" ~> opt("""[a-z][a-z]""".r) <~ "]") ^^ {
    case "B" ~ None => PassProp(Black)
    case "B" ~ Some(x) => BlackMove(x.charAt(0).toInt - 97, x.charAt(1).toInt - 97)
    case "W" ~ None => PassProp(White)
    case "W" ~ Some(x) => WhiteMove(x.charAt(0).toInt - 97, x.charAt(1).toInt - 97)
  }

  lazy val prop: Parser[SgfProp] = ident ~ cvalue ~ rep(cvalue) ^^ {
    case "C" ~ x ~ Nil => CommentProp(x)
    case "SZ" ~ x ~ Nil if wholeNum.unapplySeq(x).isDefined => SzProp(x.toInt)
    case "KM" ~ x ~ Nil => KomiProp(BigDecimal(x))
    case "RU" ~ x ~ Nil => RulesProp(x)
    case "PW" ~ x ~ Nil => Player(White, x)
    case "PB" ~ x ~ Nil => Player(Black, x)
    case i ~ x ~ y => ComplexProp(i, x :: y)
  }

  //  using {1,2} here causes a stack overflow in certain cases
  val ident = """[A-Z][A-Z]""".r | """[A-Z]""".r

  val cvalue = "[" ~> """(\\\]|[^\]])*""".r <~ "]" ^^ { x =>
    """\\\]""".r.replaceAllIn(x, _ => "]")
  }

  val wholeNum = """\d*""".r
}

trait SgfGame {
  def sgfGame(sgf: TreeLoc[SgfNode]): Option[Game] = {
    val root = sgf.root.tree.rootLabel

    root.size map { s =>
      val sgft = sgf map { n: SgfNode =>
        if
      }
    } getOrElse None
  }
}

object SGF extends SgfParser {

  def parse(reader: Reader) = parseAll(gameTree, reader)
  def parse(input: String) = parseAll(gameTree, input)
}
