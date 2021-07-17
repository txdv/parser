package bentkus.parser

sealed trait Tag
case class Open(name: String) extends Tag
case class Close(name: String) extends Tag

object Test {
  type Result[M[_], T, I] = M[(T, I)]
  type Parser[M[_], T, I] = I => Result[M, T, I]

  type StringResult[T] = Result[List, T, String]
  type StringParser[T] = String => StringResult[T]

  trait Semigroup[A] {
    def plus(x: A, y: A): A
  }

  object Semigroup {
    implicit object SemigroupInt extends Semigroup[Int] {
      def plus(x: Int, y: Int): Int = x + y
    }

    implicit object SemigroupString extends Semigroup[String] {
      override def plus(a: String, b: String): String = a + b
    }

    implicit object SemigroupOption extends Semigroup[Option[_]] {
      override def plus(a: Option[_], b: Option[_]): Option[_] = a.orElse(b)
    }
  }

  trait Semigroup2[A] {
    def multiply(x: A, y: A): A
  }

  object Semigroup2 {
    implicit object Semigroup2Int extends Semigroup2[Int] {
      def multiply(x: Int, y: Int): Int = x * y
    }
  }

  import scala.reflect.ClassTag

  trait Monad[M[_]] {
    def unit[A](a: A): M[A]
    def bind[A, B](m: M[A])(f: A => M[B]): M[B]
  }

  object Monad {
    implicit def syntax[M[_], A, B](m: M[A])(implicit tc: Monad[M]) = new {
      def map[B](f: A => B): M[B] = tc.bind(m)((a) => tc.unit(f(a)) )
      def flatMap[B](f: A => M[B]): M[B] = tc.bind(m)(f)
    }

    implicit object option extends Monad[Option] {
      override def unit[A](a: A) = Some(a)
      override def bind[A, B](opt: Option[A])(f: A => Option[B]) = opt flatMap f
    }


    implicit object list extends Monad[List] {
      override def unit[A](a: A) = List(a)
      override def bind[A, B](opt: List[A])(f: A => List[B]) = opt flatMap f
    }

  }

  import Monad._

  implicit object MonadicParser extends Monad[StringParser] {
    override def unit[A](a: A) = { input: String => List((a, input)) }
    override def bind[A, B](p: StringParser[A])(f: A => StringParser[B]) = { input: String =>
      for {
        (value1, input1) <- p(input)
        result <- f(value1)(input1)
      } yield result
    }
  }

  implicit object SemigroupStringParser extends Semigroup[StringParser[String]] {
    override def plus(a: StringParser[String], b: StringParser[String]): StringParser[String] = {
      null
    }
  }

  def ident(str: String): StringParser[String] = { input: String =>
    if (input.startsWith(str)) {
      List((str, input.substring(str.length)))
    } else {
      List.empty
    }
  }

  def sum[M[_], A](mx: M[A], my: M[A])(implicit m: Monad[M], s: Semigroup[A]): M[A] = {
    for {
      x <- mx
      y <- my
    } yield s.plus(x, y)
  }

  def multiply[M[_], A](mx: M[A], my: M[A])(implicit m: Monad[M], s: Semigroup2[A]): M[A] = {
    for {
      x <- mx
      y <- my
    } yield s.multiply(x, y)
  }

  def or[M[_]](mx: M[_], my: M[_])(implicit s: Semigroup[M[_]]): M[_] = {
    s.plus(mx, my)
  }

/*
  implicit object SemigroupStringParserString extends Semigroup[StringParser[_]] {
    def plus(x: StringParser[_], y: StringParser[_]): StringParser[_] = { input: String =>
      x(input) ++ y(input)
    }
  }
  */

  def main(args: Array[String]): Unit = {

    println(sum(Option(1), Option(2)))
    println(or(Option(1), Option(2)))

    //val c = or(or(sum(ident("a"), ident("b")), ident("b")), ident("a"))
    val a = ident("a")
    val b = ident("b")
    val ab = sum(a, b)

a    //val c = or(or(a, b), ab)

    println(a)
    println(ab)

    val g: StringParser[String] = for {
      v <- ident("a")
      _ <- ident("_")
    } yield v

    println(g("a_"))
  }
}
