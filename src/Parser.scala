package bentkus.parser

object Parser {
  trait Monad[F[_], T] {
    def map[S](f: T => S): F[S]

    def flatMap[S](f: T => F[S]): F[S]
  }

  trait Semigroup[A] {
    def ++(x: => A): A
  }

  def bind[T, S](p: Parser[T], f: T => Parser[S])(input: String): Result[S] = {
    for {
      (value1, input1) <- p(input)
      result <- f(value1)(input1)
    } yield result
  }

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

  def plus[T](p: Parser[T], q: => Parser[T])(input: String): Result[T] = {
    p(input) ++ q(input)
  }
    
  implicit class ParserSemigroup[T](p: Parser[T]) extends Semigroup[Parser[T]] {
    def ++(q: => Parser[T]): Parser[T] = plus(p, q)
  }

  val item: Parser[Char] = { input: String =>
    if (input.length == 0) {
      List.empty
    } else {
      List((input(0), input.substring(1)))
    }
  }

  def eval[T](result: Result[T]): Option[T] = {
    result.find { case (value, list) => list.size == 0 }.map(_._1)
  }


  def result[T](v: T): Parser[T] = (input: String) => List((v, input))

  def zero[T]: Parser[T] = (input: String) => List.empty


  implicit class ParserWithFilter(p: Parser[Char]) {
    def withFilter(p: Char => Boolean): Parser[Char] = for {
      x <- item
      y <- if (p(x)) result(x) else zero
    } yield y
  }

  def sat(p: Char => Boolean): Parser[Char] = {
    for {
      x <- item if p(x)
    } yield x
  }

  def char(ch: Char): Parser[Char] = sat(_ == ch)

  val digit = sat(ch => ch >= '0' && ch <= '9')
  val lower = sat(ch => ch >= 'a' && ch <= 'z')
  val upper = sat(ch => ch >= 'A' && ch <= 'Z')

  val letter = plus(lower, upper)(_)

  val alphanum = plus(letter, digit)(_)

  def ident(str: String): Parser[String] = { input: String =>
    if (input.startsWith(str)) {
      List((str, input.substring(str.length)))
    } else {
      List.empty
    }
  }

  def many[T](p: Parser[T]): Parser[List[T]] = {
    val res = for {
      x <- p
      xs <- many(p)
    } yield {
      x :: xs
    }

    res ++ result(List.empty)
  }

  def many1[T](p: Parser[T]): Parser[List[T]] = {
    for {
      x <- p
      xs <- many(p)
    } yield x :: xs
  }

  def nat: Parser[Int] = {
    for {
      xs <- many1(digit)
    } yield {
      xs.map(_.toInt - '0'.toInt).foldLeft(0) { case (m, n) => 10 * m + n }
    }
  }

  def int: Parser[Int] = {
    (for {
      _ <- char('-')
      n <- nat
    } yield -n) ++ nat
  }


  def sepBy1[T, S](p: Parser[T], sep: Parser[S]): Parser[List[T]] = for {
    x <- p
    xs <- many {
      for {
        _ <- sep
        y <- p
      } yield y
    }
  } yield x :: xs

  val `[` = char('[')
  val `]` = char(']')
  val `,` = char(',')
  val `(` = char('(')
  val `)` = char(')')

  def bracket[A, B, C](open: Parser[A], p: Parser[B], close: Parser[C]): Parser[B] = for {
    _ <- open
    x <- p
    _ <- close
  } yield x

  val ints: Parser[List[Int]] = bracket(`[`, sepBy1(int, `,`), `]`)

  lazy val expr: Parser[Int] = chainl1(factor, addop2)

  def chainl1[T](p: Parser[T], op: Parser[(T, T) => T]): Parser[T] = {
    def rest(x: T): Parser[T] =
      op.flatMap(f => p.flatMap(y => rest(f(x, y)))) ++ result(x)

    p.flatMap(rest)
  }

  lazy val `op+`: Parser[(Int, Int) => Int] = for { _ <- char('+') } yield (_ + _)
  lazy val `op-`: Parser[(Int, Int) => Int] = for { _ <- char('-') } yield (_ - _)

  lazy val addop: Parser[(Int, Int) => Int] = `op+` ++ `op-`
  lazy val factor: Parser[Int] = nat ++ bracket(`(`, expr, `)`)

  def chainr1[T](p: Parser[T], op: Parser[(T, T) => T]): Parser[T] = {
    p.flatMap(x => {
      val t = (for {
        f <- op
        y <- chainr1(p, op)
      } yield f(x, y))

      t ++ result(x)
    })
  }

  def ops[T, S](xs: List[(Parser[T], S)]): Parser[S] = {
    val r = for {
      (p, op) <- xs
    } yield {
      for { _ <- p } yield op
    }

    r.reduceRight(_ ++ _)
  }

  lazy val addop2: Parser[(Int, Int) => Int] = ops(List(
    (char('+'), (a: Int, b: Int) => a + b),
    (char('-'), (a: Int, b: Int) => a - b),
  ))

}
