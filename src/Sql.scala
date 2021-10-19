package bentkus.parser

import Parser._

import scala.reflect.ClassTag

case class Query(select: Select, from: From)

case class Table(name: String)

case class Select(elements: Seq[Select.Element])
case object Select {
  sealed trait Element
  case object Star extends Element
  case class Ident(name: String) extends Element

  def apply(elements: Seq[String])(implicit classTag: ClassTag[Seq[String]]): Select = Select {
    elements.map {
      case "*" => Select.Star
      case name => Select.Ident(name)
    }
  }
}

case class From(tables: Seq[String])

sealed trait Expr

case object Expr {
  case class Number(value: String) extends Expr
  case class Plus(value: String) extends Expr
  case class Ident(name: String) extends Expr
  case class Equal(left: Expr, right: Expr) extends Expr
  case class Sum(left: Expr, right: Expr) extends Expr
}

case class Where(expr: Expr)

object Sql {
  val ws = Seq(' ', '\t', '\n')

  def token[T](p: Parser[T]): Parser[T] = for {
    _ <- many(sat(ws.contains))
    r <- p
  } yield r

  val word = token {
    take { ch =>
      ch.isLetterOrDigit || ch == '_'
    }
  }

  val select = for {
    _ <- token(ident("select"))
    names <- sepBy1(word, `,`)
  } yield Select(names)

  val from = for {
   _ <- token(ident("from"))
    table <- sepBy1(word, `,`)
  } yield From(table)

  val number = for {
    num <- token(ident("1"))
  } yield Expr.Number(num)

  val identifier = for {
    value <- token(take(_.isLetterOrDigit))
  } yield Expr.Ident(value)

  val equal = for {
    left <- identifier
    _ <- token(char('='))
    right <- number
  } yield Expr.Equal(left, right)

  val exp = for {
    e <- equal
  } yield e

  val where = for {
    _ <- token(ident("where"))
    e <- exp
  } yield {
    Where(e)
  }

  val query = for {
    s <- select
    f <- from
  } yield Query(s, f)


}