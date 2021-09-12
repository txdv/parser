package bentkus.parser.bazel

import bentkus.parser.Parser
import org.scalatest._
import flatspec._
import matchers._

import Parser._
import Bazel._
import Ast._

class ParserSpec extends AnyFlatSpec with should.Matchers {
  "bazel" should "parse whitespace" in {
    eval(whitespace("")) should be(Some(""))
    eval(whitespace(" ")) should be(Some(" "))
  }

  "bazel" should "parse simple definitions" in {
    eval(identifier("text")) should be (Some("text"))
    eval(identifier("123")) should be (Some("123"))
  }

  "bazel" should "parse number" in {
    eval(number("123")) should be(Some(Num(123)))
  }

  "bazel" should "parse identifier" in {
    eval(identifier(" 123")) should be(Some("123"))
    eval(identifier("\t\n123")) should be(Some("123"))
  }

  "bazel" should "parse decl" in {
    eval(valDecl("a = 123")) should be(Some {
      ValDecl("a", Num(123))
    })
  }

  "bazel" should "parse function call" in {
    eval(exp("func(123)")) should be(Some(Func("func", Seq(Num(123)))))
    eval(exp("123")) should be(Some(Num(123)))
    eval(exp("asd")) should be(Some(Ident("asd")))
  }

}
