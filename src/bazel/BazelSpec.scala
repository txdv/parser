package bentkus.parser.bazel

import bentkus.parser.{Parser, IO}
import org.scalatest._
import flatspec._
import matchers._

import Parser._
import Bazel._
import Ast._
import Ast.Conversions._

class ParserSpec extends AnyFlatSpec with should.Matchers {
  "bazel" should "parse whitespace" in {
    eval(whitespace("")) should be(Some(""))
    eval(whitespace(" ")) should be(Some(" "))
  }

  "bazel" should "parse simple definitions" in {
    eval(identifier("text")) should be (Some("text"))
    eval(identifier("a123")) should be (Some("a123"))
  }

  "bazel" should "parse number" in {
    eval(number("123")) should be(Some(Num(123)))
  }

  "bazel" should "parse identifier" in {
    eval(identifier("a123")) should be(Some("a123"))
    eval(identifier("\t\na123")) should be(Some("a123"))
  }

  "bazel" should "parse decl" in {
    eval(valDecl("a = 123")) should be(Some {
      ValDecl("a", Num(123))
    })
  }

  "bazel" should "parse function call" in {
    eval(exp("func(123)")) should be(Some {
      Func("func", Seq(Func.Arg.Pure(Num(123))))
    })

    eval(exp("func(a = 123)")) should be(Some {
      Func("func", Seq(Func.Arg.Named("a", Num(123))))
    })
  }

  "bazel" should "parse arr" in {
    eval(arr("[1,2,3]")) should be(Some {
      Arr(Seq(Num(1), Num(2), Num(3)))
    })

    eval(arr("[1,2,3,]")) should be(Some {
      Arr(Seq(Num(1), Num(2), Num(3)))
    })
  }

  "bazel" should "parse methods on expressions" in {
    eval(exp("123.call(1)")) should be(Some {
      Method(Num(123), Func("call", Seq(Num(1))))
    })
  }

  "bazel" should "parse comments" in {
    eval(comment("#some comment\n")) should be(Some {
      Comment("some comment")
    })
  }

}
