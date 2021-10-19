package bentkus.parser

import bentkus.parser.Parser.eval
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class SqlSpec extends AnyFlatSpec with should.Matchers {

  "from" should "parse listed tables" in {
    eval(Sql.from("from a, b, c")).get should be(From(Seq("a", "b", "c")))
  }

  "from" should "parse difficult table names" in {
    eval(Sql.from("from send_requests")).get should be(From(Seq("send_requests")))
  }

  "select" should "parse simple name" in {
    eval(Sql.select("select a")).get should be(Select(Seq("a")))
  }

  "query" should "parse simple query" in {
    eval(Sql.query("select a from b")).get should be(Query(Select(Seq("a")), From(Seq("b"))))
  }

  "where" should "parse simple quality" in {
    eval(Sql.where("where a = 1")).get should be(Where(Expr.Equal(Expr.Ident("a"), Expr.Number("1"))))
  }

  "where" should "parse complex equality" in {
    import Expr._
    eval(Sql.where("where a + 1 = 2 + b")).get should be(Where {
      Equal(
        Sum(Ident("a"), Number("1")),
        Sum(Number("2"), Ident("b"))
      )
    })
  }
}
