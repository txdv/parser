package bentkus.parser

sealed trait Tag
case class Open(name: String) extends Tag
case class Close(name: String) extends Tag

object Test {
  type Result[T] = List[(T, String)]
  type Parser[T] = String => Result[T]

/*
  trait Semigroup[U, -T, +S] {
    def ++(x: T): S
  }*/

/*
  trait Semigroup[U, -T, +S] {
    def ++(x: T): S
  }*/

  trait Monad[F[_], +T] {
    def map[S](f: T => S): F[S]

    def flatMap[S](f: T => F[S]): F[S]
  }

  val item: Parser[Char] = { input: String =>
    if (input.length == 0) {
      List.empty
    } else {
      List((input(0), input.substring(1)))
    }
  }

  val open: Parser[Open] = item.map(i => Open(i.toString))
  val close: Parser[Close] = item.map(i => Close(i.toString))


  def plus[T](p: Parser[T], q: => Parser[T])(input: String): Result[T] = {
    p(input) ++ q(input)
  }

  def bind[T, S](p: Parser[T], f: T => Parser[S])(input: String): Result[S] = {
    for {
      (value1, input1) <- p(input)
      result <- f(value1)(input1)
    } yield result
  }

/*
  implicit class ParserSemigroup[U[_] <: Parser[_], T[_] <: Parser[_], S[_] <: Parser[_]](p: U[_]) extends Semigroup[U[_], T[_], S[_]] {
    def ++(q: T[_]): S[_] = { input: String =>
      p(input)
    }
  }*/

  implicit class ParserMonad[T](p: Parser[T]) extends Monad[Parser, T] {
    def map[S](f: T => S): Parser[S] = {
      p(_).map {
        case (result, rest) => (f(result), rest)
      }
    }

    def flatMap[S](f: T => Parser[S]): Parser[S] = {
      bind(p, f)
    }
  }
    

  def main(args: Array[String]): Unit = {
    //println(open ++ close)

  }


}
