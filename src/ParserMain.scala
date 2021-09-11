package bentkus.parser

import Parser._
import Span.Conversions._

sealed trait Html {
  val children: Seq[Html]
}

case class CloseTag(name: String, children: Seq[Html] = Seq.empty)

object Main {
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


  def closeTag(string: String) = for {
    _ <- `<`
    _ <- `/`
    name <- ident(string)
    _ <- `>`
  } yield name

  def main(args: Array[String]): Unit = {
    (1 to 100000000).foreach { i =>

      ident("true")("true")
    }
  }
}
