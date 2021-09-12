package bentkus

package object parser {
  case class Span(array: Array[Byte], offset: Int, end: Int) {
    val size: Int = end - offset

    val length: Int = end - offset

    def substring(start: Int): Span = {
      if (length > 0) {
        Span(array, offset + start, end)
      } else {
        throw new IllegalStateException
      }
    }

    def index(i: Int): Byte = array(offset + i)

    def startsWith(other: Span): Boolean = {
      if (size <= other.size) {
        /*
        var i = 0
        while (i < size) {
          if (index(i) != other.index(i)) {
            return false
          }
          i += 1
        }
        true*/
       java.util.Arrays.compare(
           array, offset, end,
           other.array, other.offset, other.end) == 0
      } else {
        true
      }
    }

    def char(offset: Int): Char = {
      array(this.offset + offset).toChar
    }

    def takeBytesWhile(isByte: Byte => Boolean): Span = {
      var i = 0
      while (i < size && isByte(index(i))) {
        i += 1
      }

      Span(array, offset, end - size + i)
    }

    def takeWhile(ch: Char => Boolean): Span = {
      takeBytesWhile(byte => ch(byte.toChar))
    }

    def string: String = {
      new String(array, offset, end - offset)
    }

    override def toString: String = {
      s"Span('$string', $offset, $end)"
    }

    override def equals(other: Any): Boolean = {
      other match {
        case span: Span =>
          if (size == span.size) {
            startsWith(span)
          } else false
        case string: String =>
          equals(Span(string))
      }
    }
  }

  object Span {
    def apply(string: String): Span = {
      val bytes = string.getBytes
      Span(bytes, 0, bytes.length)
    }

    object Conversions {
      implicit def string2Span(string: String): Span = Span(string)
      implicit def Span2String(span: Span): String = span.string
    }
  }

  type Result[T] = List[(T, Span)]
  type Parser[T] = Span => Result[T]

}
