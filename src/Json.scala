package bentkus.parser

import Parser._

sealed trait JObject

/*
object JObject {
  def apply(value: Boolean): JObject = JBool(value)

  def apply(value: Int): JObject = JInt(value)

  def apply(value: String): JObject = JString(value)
}
*/

case class JBool(value: Boolean) extends JObject
case class JInt(value: Int) extends JObject
case class JString(value: String) extends JObject
case class JArray(value: Seq[JObject]) extends JObject
case class JMap(map: Map[String, JObject]) extends JObject

object Json {
  val `true` = for {
    _ <- ident("true")
  } yield JBool(true)

  val `false` = for {
    _ <- ident("false")
  } yield JBool(false)

  val jbool = `true` ++ `false`

  val jint = for {
    i <- int
  } yield JInt(i)

  val `"` = char('"')
  val jstring = for {
    str <- bracket(`"`, take(_ != '"'), `"`)
  } yield JString(str.mkString)

  val jarray: Parser[JArray] = for {
    _ <- `[`
    arr <- sepBy1(jexpr, char(','))
    _ <- `]`
  } yield JArray(arr)

  val jKeyValue: Parser[(String, JObject)] = for {
    key <- jstring
    _ <- char(':')
    value <- jexpr
  } yield key.value -> value

  val jmap: Parser[JMap] = for {
    _ <- char('{')
    keyValues <- jKeyValue
    _ <- char('}')
  } yield JMap(Map(keyValues))

  val jexpr: Parser[JObject] = plus(plus(plus(plus(jbool, jint), jstring), jarray), jmap)
  //val jexpr: Parser[JObject] = jbool +++ jint +++ jstring +++ jarray +++ jmap
}
