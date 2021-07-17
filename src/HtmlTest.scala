package bentkus.parser

sealed trait Style
sealed trait Color extends Style

object Color {
  case object Green extends Color
  case object Red extends Color
  case object Yellow extends Color
  case object Blue extends Color
}

sealed trait TStyle extends Style
object TStyle {
  case object Bold extends TStyle
  case object Underline extends TStyle
  case object Italic extends TStyle
  case object Blink extends TStyle
}

object IO {
  def read(filename: String): String = {
    scala.io.Source.fromFile(filename).mkString.trim
  }
}

case class Rule(tagName: String, styles: Seq[Style])

object HtmlTest {

  val bold = "\u001B[1m"
  val nobold = "\u001B[22m"

  val italic = "\u001b[3m"
  val noitalic = "\u001b[23m"

  val underline = "\u001b[4m"
  val nounderline = "\u001b[24m"

  val default = "\u001b[39m"

  val blink = "\u001b[5m"
  val noblink = "\u001b[25m"

  def styleRules(tag: Html): Seq[Rule] = tag match {
    case text: Text =>
      val results = Parser.eval(Css.definitions(text.text)).toSeq

      for {
        result <- results
        definition <- result
      } yield definition match {
        case (name, stringStyles) =>
          val styles = stringStyles.flatMap(style)
          Rule(name, styles)
      }
    case _ =>
      Seq.empty
  }

  var styling: Seq[Rule] = Seq.empty

  def format(text: String): Unit = {
    val html = Parser.eval(Html.html(text)).getOrElse {
      throw new Exception("can't parse html")
    }

    styling = jq("html.head.style", html).flatMap(styleRules)

    jq("html.body", html).foreach(format)
  }

  def style(input: String): Seq[Style] = {
    Parser.eval(Css.keyValues(input)).toSeq.flatMap { rules =>
      rules.flatMap(style)
    }
  }

  def style(input: (String, String)): Seq[Style] = {
    val empty = Seq.empty

    input match {
      case ("color", color) =>
        color match {
          case "green"  => Seq(Color.Green)
          case "red"    => Seq(Color.Red)
          case "yellow" => Seq(Color.Yellow)
          case "blue"   => Seq(Color.Blue)
          case _        => empty
        }
      case ("font-weight", "bold") => Seq(TStyle.Bold)
      case ("text-decoration", "underline") => Seq(TStyle.Underline)

      case (_, _) => empty
    }
  }

  def terminal(style: Style): String = {
    style match {
      case Color.Green      => Console.GREEN
      case Color.Red        => Console.RED
      case Color.Yellow     => Console.YELLOW
      case Color.Blue       => Console.BLUE
      case TStyle.Bold      => bold
      case TStyle.Underline => underline
      case TStyle.Italic    => italic
      case TStyle.Blink     => blink
    }
  }

  def setup(styles: Seq[Style]): Unit = {
    styles.foreach(setup)
  }

  def setup(style: Style): Unit = {
    print(terminal(style))
  }

  def cleanup(styles: Seq[Style]): Unit = {
    styles.foreach(cleanup)
  }

  def st(style: Style)(f: => Unit): Unit = {
    st(Seq(style))(f)
  }

  def st(styles: Seq[Style])(f: => Unit): Unit = {
    setup(styles)
    f
    cleanup(styles)
  }

  def cleanup(style: Style): Unit = {
    val code = style match {
      case _: Color => default
      case TStyle.Bold => nobold
      case TStyle.Underline => nounderline
      case TStyle.Italic => noitalic
      case TStyle.Blink => noblink
    }
    print(code)
  }

  def styleFromTag(name: String): Seq[TStyle] = {
    name match {
      case "b" => Seq(TStyle.Bold)
      case "u" => Seq(TStyle.Underline)
      case "i" => Seq(TStyle.Italic)
      case _   => Seq.empty
    }
  }

  val ws = Seq(' ', '\t', '\n', '\r')

  def onlySpaces(str: String): Boolean = {
    str.forall(ws.contains)
  }

  def format(html: Html): Unit = {
    html match {
      case Text(text) =>
        if (!onlySpaces(text)) {
          print(text.trim)
          print(" ")
        }
      case Tag(name, attr, children) =>
        name match {
          case "h1" | "h2" | "p" => println
          case _ =>
        }
        val styles = styling.find(_.tagName == name).toSeq.flatMap(_.styles)

        val stylesTags = styleFromTag(name)
        val stylesAttrs = attr.get("style").toSeq.flatMap(style)
        st(styles ++ stylesTags ++ stylesAttrs) {
          children.foreach(format)
        }

        name match {
          case "h1" | "h2" | "p" => println
          case _ =>
        }
    }
  }

  def jq(path: String, html: Seq[Html]): Seq[Html] = {
    jq(path.split("\\.").toList, html)
  }

  def jq(names: List[String], html: Seq[Html]): Seq[Html] = {
    names match {
      case x :: xs =>
        val res = html.filter {
          case tag: Tag =>
            tag.name == x
          case _ => false
        }.flatMap(_.children)

        jq(xs, res)
      case Nil =>
        html
    }
  }

  def main(args: Array[String]): Unit = {
    args.foreach { arg =>
      format(IO.read(arg))
    }
  }
}
