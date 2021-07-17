package bentkus.parser

import org.scalatest._
import flatspec._
import matchers._

import bentkus.parser.Tag // override specs2 tag definition

import Parser._
import Html._

class HtmlSpec extends AnyFlatSpec with should.Matchers {
  "openTag" should "parse html open tag" in {
    eval(openTag("<html")) should be(Some("html"))
  }

  "closeTag" should "parse html close tag" in {
    eval(closeTag("html")("</html>")) should be(Some(CloseTag("html")))
  }

  "tag" should "parse html tags" in {
    eval(tag("<html/>")) should be(Some(Tag("html")))
    eval(tag("<html></html>")) should be(Some(Tag("html")))
    eval(tag("""<html a="a"></html>""")) should be(Some {
      Tag("html", attr = Map("a" -> "a"))
    })
    eval(tag("<html><sub/></html>")) should be(Some {
      Tag("html").child(Tag("sub"))
    })

    eval(tag("""<a a="1 = 2"><b b="2 + 3"/></a>""")) should be(Some {
      Tag("a").attr("a", "1 = 2").child {
        Tag("b").attr("b", "2 + 3")
      }
    })
  }

  "html" should "parse text and html tags" in {
    eval(html("stand alone text")) should be(Some(List(Text("stand alone text"))))
    eval(html("<b>bolded text</b>")) should be(Some(List {
      Tag("b").child(Text("bolded text"))
    }))
  }
}
