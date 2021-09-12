package bentkus.parser.bazel

import bentkus.parser.Parser
import bentkus.parser.Parser._

sealed trait Ast

object Ast {
  case class Num(number: Int) extends Ast
  case class ValDecl(name: String, value: Num) extends Ast
  case class Func(name: String, expr: Seq[Ast]) extends Ast
  case class Ident(value: String) extends Ast
}

object Bazel {
  import Ast._

  val whitespace = take(_.isWhitespace) ++ result("")

  def token[T](parser: Parser[T]): Parser[T] = for {
    _ <- whitespace
    result <- parser
  } yield result

  val identifier = token(take(_.isLetterOrDigit))

  val `=` = token(char('='))
  val `(` = token(char('('))
  val `)` = token(char(')'))
  val `,` = token(char(','))

  lazy val number: Parser[Num] = token {
    for {
      result <- take(_.isDigit)
    } yield Num(result.toInt)
  }

  lazy val ident =
    for {
      value <- identifier
    } yield Ident(value)

  lazy val funcCall: Parser[Ast] = token {
    for {
      name <- identifier
      _ <- `(`
      args <- sepBy1(exp, `,`)
      _ <- `)`
    } yield Func(name, args)
  }

  lazy val exp = funcCall ++ number ++ ident

  val valDecl = for {
    name <- identifier
    sign <- `=`
    n <- number
  } yield ValDecl(name, n)

}
