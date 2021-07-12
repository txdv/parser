package bentkus

package object parser {
  type Result[T] = List[(T, String)]
  type Parser[T] = String => Result[T]

}
