package bentkus.parser

import org.scalatest._
import flatspec._
import matchers._

import Parser._
import Json._

class ParserSpec extends AnyFlatSpec with should.Matchers {
  /*
  "JObject" should "build stuff" in {
    JObject(true) should be(JBool(true))
    JObject(false) should be(JBool(false))
    JObject(1) shoudl be(JInt(1))
    JObject(1)
  }
  */

  "jnull" should "parse null" in {
    eval(jnull("null")) should be (Some(JNull))
  }

  "jbool" should "parse booleans" in {
    eval(jbool("true")) should be (Some(JBool(true)))
    eval(jbool("false")) should be (Some(JBool(false)))
    eval(jbool("string")) should be (None)
    eval(jbool("1")) should be (None)
  }

  "jint" should "parse integers" in {
    eval(jint("1")) should be(Some(JInt(1)))
    eval(jint("2")) should be(Some(JInt(2)))
    eval(jint("123")) should be(Some(JInt(123)))
  }

  "jstring" should "parse string" in {
    eval(jstring("\"a\"")) should be(Some(JString("a")))
  }

  "jarray" should "parse arrays" in {
    //eval(jarray("[]")) should be(Some(JArray(Seq.empty)))
    eval(jarray("[1]")) should be(Some(JArray(Seq(JInt(1)))))
    eval(jarray("[1,2]")) should be(Some(JArray(Seq(JInt(1), JInt(2)))))
    eval(jarray("[true,false,null]")) should be(Some(JArray(Seq(JBool(true), JBool(false), JNull))))
  }
}
