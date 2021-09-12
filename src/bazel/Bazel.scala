package bentkus.parser.bazel

import bentkus.parser.Parser
import bentkus.parser.Parser._

sealed trait Ast

object Ast {
  object Conversions {
    implicit def expr2FuncArgPure(expr: Expr): Func.Arg.Pure =
      Func.Arg.Pure(expr)

  }

  case class ValDecl(name: String, value: Expr) extends Ast

  sealed trait Expr extends Ast
  case class Num(number: Int) extends Expr

  case class Func(name: String, args: Seq[Func.Arg]) extends Expr
  object Func {
    sealed trait Arg extends Ast
    object Arg {
      case class Pure(expr: Ast) extends Arg
      case class Named(name: String, expr: Ast) extends Arg
    }
  }

  case class Ident(value: String) extends Expr
  case class Arr(values: Seq[Expr]) extends Expr
  case class Str(value: String) extends Expr

  case class Method(exp: Expr, func: Func) extends Expr
}

object Bazel {
  import Ast._

  val whitespace = takeEmpty(_.isWhitespace)

  def token[T](parser: Parser[T]): Parser[T] = for {
    _ <- whitespace
    result <- parser
  } yield result

  val identifier = token(takeFirst(_.isLetter, a => a.isLetterOrDigit || a == '_'))

  val `=` = token(char('='))
  val `(` = token(char('('))
  val `)` = token(char(')'))
  val `,` = token(char(','))
  val `[` = token(char('['))
  val `]` = token(char(']'))

  lazy val number: Parser[Num] = token {
    for {
      result <- take(_.isDigit)
    } yield Num(result.toInt)
  }

  lazy val ident =
    for {
      value <- identifier
    } yield Ident(value)

  lazy val funcArgPure: Parser[Func.Arg.Pure] = token {
    for {
      arg <- exp
    } yield Func.Arg.Pure(arg)
  }

  lazy val funcArgNamed = for {
    decl <- valDecl
  } yield Func.Arg.Named(decl.name, decl.value)

  lazy val funcArg = zero[Func.Arg] ++ funcArgPure ++ funcArgNamed

  lazy val func: Parser[Func] = token {
    for {
      name <- identifier
      _ <- `(`
      args <- sepBy1(funcArg, `,`)
      _ <- `)`
    } yield Func(name, args)
  }

  def sepByN[T](expr: Parser[T], sep: Parser[Char]) = for {
    expr <- sepBy1(exp, sep)
    _ <- opt(sep)
  } yield expr

  lazy val arr: Parser[Arr] = token {
    for {
      _ <- `[`
      expressions <- sepByN(exp, `,`)
      _ <- `]`
    } yield Arr(expressions)
  }

  lazy val string = token {
    for {
      _ <- char('"')
      value <- take(_ != '"')
      _ <- char('"')
    } yield Str(value)
  }

  lazy val exp = for {
    e <- zero[Expr] ++ number ++ ident ++ string ++ arr ++ func
    func <- opt {
      for {
        _ <- char('.')
        f <- func
      } yield f
    }
  } yield func.map { f =>
    Method(e, f)
  } getOrElse {
    e
  }

  val valDecl = for {
    name <- identifier
    _ <- `=`
    expression <- exp
  } yield ValDecl(name, expression)

  val expressions = many(exp)

  val file = for {
    result <- many(exp)
    _ <- whitespace
  } yield result

}
