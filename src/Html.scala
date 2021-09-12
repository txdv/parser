package bentkus.parser

import Parser._

import Span.Conversions._

sealed trait Html {
  val children: Seq[Html]
}

case class Tag(
  name: String,
  attr: Map[String, String] = Map.empty,
  children: Seq[Html] = Seq.empty,
) extends Html {
  def attr(key: String, value: String): Tag =
    attr(key -> value)

  def attr(kvp: (String, String)): Tag =
    copy(attr = this.attr + kvp)

  def child(element: Html): Tag = copy(children = this.children :+ element)

  def children(children: Seq[Html]): Tag = copy(children = children)
}

case class Text(text: String) extends Html {
  val children: Seq[Html] = Seq.empty
}

case class CloseTag(name: String, children: Seq[Html] = Seq.empty)

object Html {

  val `<` = char('<')
  val `>` = char('>')
  val ` ` = char(' ')
  val `/` = char('/')
  val `=` = char('=')
  val `"` = char('"')

  val space = ` `

  val word = take(_.isLetterOrDigit)

  val openTag = for {
    _ <- `<`
    name <- word
  } yield name

  val attribute = for {
    key <- word
    _ <- `=`
    _ <- `"`
    value <- take(_ != '"')
    _ <- `"`
  } yield (key.string -> value.string)

  def attributes(parse: Boolean) =
    if (parse) {
      sepBy1(attribute, space)
    } else {
      result(List.empty[(String, String)])
    }

  val prefixAttributes = for {
    spaceChar <- next2(_ == ' ')
    attr <- attributes(spaceChar.contains(' '))
  } yield attr

  def closeTag(string: String) = for {
    _ <- `<`
    _ <- `/`
    name <- ident(string)
    _ <- `>`
  } yield CloseTag(name)

  def tagTail(string: String, end: Boolean): Parser[CloseTag] =
    if (end) {
      result(string).map(_ => CloseTag(string))
    } else {
      for {
        children <- html
        close <- closeTag(string)
      } yield close.copy(children = children)
    }

  val tag = for {
    _ <- `<`
    name <- word
    attr <- prefixAttributes
    endChar <- next2(_ == '/')
    _ <- `>`
    end <- tagTail(name, endChar.contains('/'))
  } yield Tag(name, attr.toMap, end.children)

  val text = for {
    value <- take(_ != '<')
  } yield Text(value)

  val html = many(plus(tag, text) _)
}
