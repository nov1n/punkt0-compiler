package punkt0
package lexer

import java.io.File

object Lexer extends Phase[File, Iterator[Token]] {
  import Reporter._

  def run(f: File)(ctx: Context): Iterator[Token] = {
    var eof: Boolean = false
    var current : Char = ' ' // Overwritten by first call to next
    val source = scala.io.Source.fromFile(f)

    new Iterator[Token] {

      def hasNext: Boolean = {
        !eof
      }

      def next: Token = {

        def parseBad: Token = {
          // Consume iterator up to next whitespace to invalidate this token
          while(current.isWhitespace && source.hasNext) {
            current = source.next
          }
          new Token(BAD)
        }

        def parseCommentOrDiv: Token = {
          if (source.hasNext) {
            val b = new StringBuffer
            b.append(current)

            current = source.next
            if (current == '/') {

              // Advance until we find a newline
              while ((b.indexOf(scala.util.Properties.lineSeparator) == -1) && source.hasNext) {
                current = source.next
                b.append(current)
              }

              // Return the next token
              next
            } else if (current == '*') {
              while ((b.indexOf("*/") == -1) && source.hasNext) {
                current = source.next
                b.append(current)
              }

              // Return the next token
              next
            } else if (current.isWhitespace) {
              new Token(DIV) // TODO: Does not handle 5/4, need whitespace --> 5 / 4. Maybe we need to share the stringbuffer, or use peek with source.buffered.current
            } else {
              parseBad
            }
          } else {
            parseBad // One '/' followed by EOF
          }
        }

        def parseIdentOrKeword: Token = {
          val b = new StringBuffer
          b.append(current)

          // Returns an error token if malformed, None otherwise
          def errorTok(): Option[Token] = {
            if(!source.hasNext) {
              None
            } else {
              current = source.next
              if (current.isLetterOrDigit || current == '_') { // Extend token
                b.append(current)
                errorTok()
              } else if (current.isWhitespace) {
                None
              } else {
                // Unexpected char
                Some(parseBad)
              }
            }
          }

          errorTok() match {
            case Some(err) => err
            case None =>
              Keywords.lookup(b.toString) match {
                case Some(kw) => new Token(kw)
                case None => new ID(b.toString)
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

        def parseEq : Token = {
          if(source.hasNext) {
            current = source.next
            if(current.isWhitespace) {
              new Token(EQSIGN)
            } else if(current == '=') {
              new Token(EQUALS)
            } else {
              parseBad
            }
          } else {
            parseBad
          }
        }

        def parseAnd : Token = {
          if (source.hasNext) {
            current = source.next
            if (current == '&') {
              new Token(AND)
            } else {
              parseBad
            }
          } else {
            parseBad
          }
        }

        def parseOr : Token = {
          if (source.hasNext) {
            current = source.next
            if (current == '|') {
              new Token(OR)
            } else {
              parseBad
            }
          } else {
            parseBad
          }

        }

        def parseStr : Token = {
          val b = new StringBuffer
          current = source.next
          while (current != '"') {
            b.append(current)
            current = source.next
          }
          new STRLIT(b.toString)
        }

        if (!source.hasNext) {
          eof = true
          new Token(EOF)
        } else {
          current = source.next

          if (current == '/') parseCommentOrDiv
          else if (current.isLetter) parseIdentOrKeword
          else if (current.isDigit) parseIntLit
          else if (current == ':') new Token(COLON)
          else if (current == ';') new Token(SEMICOLON)
          else if (current == '.') new Token(DOT)
          else if (current == ',') new Token(COMMA)
          else if (current == '=') parseEq
          else if (current == '!') new Token(BANG)
          else if (current == '(') new Token(LPAREN)
          else if (current == ')') new Token(RPAREN)
          else if (current == '{') new Token(LBRACE)
          else if (current == '}') new Token(RBRACE)
          else if (current == '&') parseAnd
          else if (current == '|') parseOr
          else if (current == '<') new Token(LESSTHAN)
          else if (current == '+') new Token(PLUS)
          else if (current == '-') new Token(MINUS)
          else if (current == '*') new Token(TIMES)
          else if (current == '"') parseStr
          else if (current.isWhitespace) next
          else parseBad
        }
      }
    }
  }

}
