package net.semeai.go

sealed trait Property {
  val ident: String
  val content: String
}

case class GenericProperty(ident: String, content: String) extends Property

case class Comment(content: String) extends Property {
  val ident = "C"
}

case class SizeProp(size: Int) extends Property {
  val ident = "SZ"
  val content = size.toString
}
