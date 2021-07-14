package bentkus.parser

sealed trait Style
sealed trait Color extends Style

object Color {
  case object Green extends Color
  case object Red extends Color
  case object Yellow extends Color
}

sealed trait TStyle extends Style
object TStyle {
  case object Bold extends TStyle
  case object Underline extends TStyle
  case object Italic extends TStyle
}

object IO {
  def read(filename: String): String = {
    scala.io.Source.fromFile(filename).mkString.trim
  }
}

object HtmlTest {

  val bold = "\u001B[1m"
  val nobold = "\u001B[22m"

  val italic = "\u001b[3m"
  val noitalic = "\u001b[23m"

  val underline = "\u001b[4m"
  val nounderline = "\u001b[24m"

  val default = "\u001b[39m"

  def format(text: String): Unit = {
    val html = Parser.eval(Html.html(text)).getOrElse {
      throw new Exception("can't parse html")
    }

    jq(List("html", "body"), html).foreach(format)
  }
  
  def style(input: String): Seq[Style] = {
    Parser.eval(Css.keyValues(input)).toSeq.flatMap { rules =>
      rules.flatMap {
        case ("color", color) =>
          color match {
            case "green"  => Some(Color.Green)
            case "red"    => Some(Color.Red)
            case "yellow" => Some(Color.Yellow)
            case _        => None
          }
        case ("font-weight", "bold") => Some(TStyle.Bold)
        case ("text-decoration", "underline") => Some(TStyle.Underline)

        case (_, _) => None
      }
    }
  }

  def terminal(style: Style): String = {
    style match {
      case Color.Green      => Console.GREEN
      case Color.Red        => Console.RED
      case Color.Yellow     => Console.YELLOW
      case TStyle.Bold      => bold
      case TStyle.Underline => underline
      case TStyle.Italic    => italic
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
    style match {
      case _: Color =>
        print(default)
      case TStyle.Bold =>
        print(nobold)
      case TStyle.Underline =>
        print(nounderline)
      case TStyle.Italic =>
        print(noitalic)
    }
  }

  def styleFromTag(name: String): Seq[TStyle] = {
    name match {
      case "b" => Seq(TStyle.Bold)
      case "u" => Seq(TStyle.Underline)
      case "i" => Seq(TStyle.Italic)
      case _   => Seq.empty
    }
  }

  def format(html: Html): Unit = {
    html match {
      case Text(text) =>
        print(text)
      case Tag(name, attr, children) =>
        val stylesTags = styleFromTag(name)
        val stylesAttrs = attr.get("style").toSeq.flatMap(style)
        st(stylesTags ++ stylesAttrs) {
          children.foreach(format)
        }
    }
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
