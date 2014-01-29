package net.semeai.go

sealed trait Property {
  val ident: String
  val content: String
}

final case class GenericProperty(ident: String, content: String) extends Property

final case class Comment(content: String) extends Property {
  val ident = "C"
}

final case class SizeProp(size: Int) extends Property {
  val ident = "SZ"
  val content = size.toString
}
