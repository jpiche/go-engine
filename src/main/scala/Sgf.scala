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

case class SgfNode(
  props: Map[String, String] = Map.empty[String,String],
  comments: List[String] = Nil
) {
  def addComment(c: String) = SgfNode(props, comments ++ List(c))
  def addProp(p: (String, String)) = SgfNode(props + p, comments)
}

object SgfNode {
  implicit val nodeShow = new Show[SgfNode] {
    override def show(as: SgfNode) = as.toString
  }
}

trait SgfParser extends RegexParsers with PackratParsers {

  lazy val gameTree = simpleSeq | complexSeq

  lazy val complexSeq: PackratParser[TreeLoc[SgfNode]] =
    "(" ~ sequence ~ rep(simpleSeq | complexSeq) ~ ")" ^^ {
      case "(" ~ s ~ Nil ~ ")" => s
      case "(" ~ s ~ t ~ ")" => t.foldLeft(s) { (tree, node) =>
        val newTree = tree insertDownLast node.root.tree
        newTree.parent.get
      }
    }

  lazy val simpleSeq = "(" ~ sequence ~ ")" ^^ {
    case "(" ~ s ~ ")" => s
  }

  // requires at least one node in the sequence
  lazy val sequence = rep(node) ^^ {
    case x :: Nil => x.leaf.loc
    case x :: nodelist => nodelist.foldLeft(x.leaf.loc) { (t, n) =>
      t insertDownLast n.leaf
    }
  }

  lazy val node = ";" ~ rep(prop) ^^ {
    case ";" ~ x => x.foldLeft(SgfNode()) { (node, p) =>
      p match {
        case ("C", v) => node addComment v
        case (_, _) => node addProp p
      }
    }
  }
  lazy val prop = propIdent ~ propValue ^^ {
    case i ~ v => (i, v)
  }
  val propIdent = """[A-Z]+""".r
  val propValue = "[" ~ cvalue ~ "]" ^^ {
    case "[" ~ x ~ "]" => x
  }

  val cvalue = """(?:\\\]|[^\]])*""".r ^^ { x =>
    """\\\]""".r.replaceAllIn(x, _ => "]")
  }
}

object SGF extends SgfParser {

  def parse(reader: Reader) = parseAll(gameTree, reader)
  def parse(input: String) = parseAll(gameTree, input)

}
