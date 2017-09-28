package punkt0
package lexer

import java.io.File

object Lexer extends Phase[File, Iterator[Token]] {
  import Reporter._

  def run(f: File)(ctx: Context): Iterator[Token] = {
    val source = scala.io.Source.fromFile(f)

    new Iterator[Token] {
      var eof: Boolean = false
      var current = ' ' // Will be overwritten on first 'next' call

      def hasNext: Boolean = {
        !eof
      }

      def next: Token = {
        if (!source.hasNext) {
          eof = true
          new Token(EOF)
        } else {
          current = source.next()

          if (current == '/') parseComment
          else if (current.isLetter) parseIdentAndKeword
          else if (current.isDigit) parseIntLit
          else if (current.isWhitespace) next
          else  throw new RuntimeException(current + " -- NO TOKEN")
        }
      }

      def parseComment: Token = {
        if (source.hasNext) {
          val b = new StringBuffer
          b.append(current)

          current = source.next
          if (current == '/') {

            // Advance until we find a newline
            while ((b.indexOf(scala.util.Properties.lineSeparator) == -1) && source.hasNext) {
              current = source.next
            }

            // Return the next token
            next
          } else  if (current == "*") {
            // TODO: Block comments
            new Token(BAD)
          }else {
            new Token(EOF) // TODO: True?
          }
        } else {
          new Token(BAD) // One '/' followed by EOF
        }
      }

      def parseIdentAndKeword: Token = {
        val b = new StringBuffer
        b.append(current)
        while (source.hasNext) {
          current = source.next
          if (current.isLetterOrDigit || current == '_') { // Extend token
            b.append(current)
          } else if (current == ' ') { // End of token
            Keywords.lookup(b.toString) match {
              case Some(kw) => return new Token(kw)
              case None => return new ID(b.toString)
            }
          } else { // Unexpected character
            new Token(BAD)
          }
        }
      }

      def parseIntLit: Token = {
        val b = new StringBuffer
        while (current.isDigit) {
          b.append(current)
          current = source.next
        }
        new INTLIT(b.toString.toInt)
      }
    }
  }
}
