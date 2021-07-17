package bentkus.parser

import org.scalatest._
import flatspec._
import matchers._

import Parser._
import Css._

class CssSpec extends AnyFlatSpec with should.Matchers {
  "css" should "parse key value pair" in {
    eval(keyValue("color :blue;")) should be(Some("color" -> "blue"))
  }

  "css" should "parse key value" in {
    eval(keyValues("color:blue;font-weight:bold;")) should be(Some {
      Seq("color" -> "blue", "font-weight" -> "bold")
    })
  }

  "css" should "parse spaces" in {
    eval(keyValues("color: blue; font-weight   :  \t bold;")) should be(Some {
      Seq("color" -> "blue", "font-weight" -> "bold")
    })
  }

  "css" should "parse basic css rule with spaces"  in {
    eval(keyValues("color: blue; font-weight   :  \t bold;")) should be(Some {
      Seq("color" -> "blue", "font-weight" -> "bold")
    })
  }
  "css" should "parse ident" in {
    eval(ident2("""p""")) should be(Some("p"))
  }

  "css" should "parse rule" in {
    val rule = ("p", Seq("color" -> "blue", "font-weight" -> "bold"))
    eval(definition("""p {color:blue;font-weight:bold;}""")) should be(Some(rule))
    eval(definition("""p {
      color:blue;
      font-weight:bold;
    }""")) should be(Some(rule))

    eval(definition("""h1 {color:blue;}""")) should be(Some {
      ("h1", Seq("color" -> "blue"))
    })

    //eval(d("""h1 { color: blue }""")) should be(Some(
  }
}
