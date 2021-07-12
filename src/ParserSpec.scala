package bentkus.parser

import org.scalatest._
import flatspec._
import matchers._

import Parser._

class ParserSpec extends AnyFlatSpec with should.Matchers {
  "item" should "parse singular char string" in {
    eval(item("1")) should be (Some('1'))
  }

  "item" should "parse double digit" in {
    eval(item("12")) should be (None)
  }

  "value" should "put value into" in {
    eval(result(1)("")) should be(Some(1))
  }

  "zero" should "be none" in {
    eval(zero[Int]("something")) should be(None)
  }

  "sat" should "parse char in condition" in {
    eval(sat(_ == 'A')("A")) should be(Some('A'))
  }

  "sat" should "not parse char in condition" in {
    eval(sat(_ == 'A')("B")) should be(None)
  }

  "char" should "parse specified char" in {
    eval(char('A')("A")) should be(Some('A'))
  }

  "char" should "not parse unspecified char" in {
    eval(char('A')("B")) should be(None)
  }

  "digit" should "parse digits" in {
    (0 to 9).map(i => ('0'.toInt + i).toChar).foreach { ch =>
      eval(digit(ch.toString)) should be(Some(ch))
    }
  }

  "++" should "combine parsers" in {
    eval((upper ++ lower)("A")) should be(Some('A'))
  }
  
  "alphanum" should "parse letters and numbers" in {
    eval(alphanum("A")) should be(Some('A'))
    eval(alphanum("a")) should be(Some('a'))
    eval(alphanum("1")) should be(Some('1'))
  }

  "ident" should "parse specified identifier" in {
    eval(ident("asd")("asd"))should be(Some("asd"))
    eval(ident("asd")("asb"))should be(None)
  }

  "many" should "parse a parser multiple times" in {
    eval(many(digit)("123")) should be(Some(List('1', '2', '3')))
  }

  "many" should "succeed with zero items" in {
    eval(many(digit)("")) should be(Some(List()))
  }

  "many1" should "succed only with at least 1 item" in {
    eval(many1(digit)("")) should be(None)
    eval(many1(digit)("1")) should be(Some(List('1')))
  }

  "nat" should "parse natural numbers" in {
    eval(nat("1")) should be(Some(1))
    eval(nat("123")) should be(Some(123))
    eval(nat("asd")) should be(None)
  }

  "int" should "parse negative numbers as well" in {
    eval(int("123")) should be(Some(123))
    eval(int("-123")) should be(Some(-123))
  }

  "sepBy1" should "parse strings with seperators" in {
    eval(sepBy1(int, `,`)("1,2,3")) should be(Some(List(1, 2, 3)))
  }

  "special chars" should "parse special chars" in {
    eval(`[`("[")) should be(Some('['))
    eval(`]`("]")) should be(Some(']'))
    eval(`,`(",")) should be(Some(','))
    eval(`)`(")")) should be(Some(')'))
    eval(`(`("(")) should be(Some('('))
  }

  "bracket" should "enclose parser with brackets" in {
    eval(bracket(`(`, int, `)`)("(123)")) should be(Some(123))
  }
  
  "ints" should "parse ints enclosed with [] and seperated by commas" in {
    eval(ints("[1,2,3,42]")) should be(Some(List(1, 2, 3, 42)))
  }

  "addop" should "create sum or minus in parser" in {
    eval(addop("+")).map(f => f(1, 2)) should be(Some(3))
    eval(addop("-")).map(f => f(1, 2)) should be(Some(-1))
  }

  "expr" should "evaluate simple expression" in {
    eval(expr("1")) should be(Some(1))
  }
}
