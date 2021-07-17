package bentkus.parser

import Parser._

class RA(ident: String, rules: Seq[(String, String)])

object Css {
  val ws = Seq(' ', '\t', '\n')

  def token[T](p: Parser[T]) = for {
    _ <- many(sat(ws.contains))
    r <- p
  } yield r

  val `:` = token(char(':'))
  val `;` = token(char(';'))

  val word = take(ch => ch.isLetterOrDigit || ch == '-')

  val key = word

  val value = word

  val keyValue = for {
    k <- token(key)
    _ <- `:`
    v <- token(value)
    _ <- `;`
  } yield (k, v)

  val ident2 = sepBy1(word, char('.')).map(_.mkString("."))

  val keyValues = many(keyValue)

  val definition = for {
    name <- token(ident2)
    _ <- token(char('{'))
    vs <- keyValues
    _ <- token(char('}'))
  } yield (name, vs)

  val definitions = for {
    res <- many(token(definition))
    _ <- many(sat(ws.contains))
  } yield res
}
