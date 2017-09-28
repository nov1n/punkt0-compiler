package punkt0
package lexer

import java.io.File

// TODO: Disallow nexted block comments
// TODO: Add positions
// TODO: Add error reporting
// TODO: Quit(1) on error
// TODO: Keep parsing on error

object Lexer extends Phase[File, Iterator[Token]] {
  import Reporter._

  def run(f: File)(ctx: Context): Iterator[Token] = {
    var eof: Boolean = false
    val source = scala.io.Source.fromFile(f).buffered // Buffered for head (peek)

    new Iterator[Token] {

      def hasNext: Boolean = {
        !eof
      }

      def next: Token = {

        def parseBad: Token = new Token(BAD)

        // TODO: Remove while loops
        def parseCommentOrDiv(b : StringBuffer) : Token = {
          source.headOption match {
            case Some(x) if x == '/' =>
              // Advance until we find a newline
              while ((b.indexOf(scala.util.Properties.lineSeparator) == -1) && source.hasNext) {
                b.append(source.next)
              }

              // Return the next token
              next
            case Some(x) if x == '*' =>
              // Advance until we find the */
              while ((b.indexOf("*/") == -1) && source.hasNext) {
                b.append(source.next)
              }

              // Return the next token
              next
            case Some(_) =>
              new Token(DIV)
            case None =>
              parseBad // TODO: it is not really bad but we have to return a token :/
          }
        }

        // TODO: Refactor errorTok
        def parseIdentOrKeword(b : StringBuffer) : Token = {
          // Returns an error token if malformed, None otherwise
          def errorTok(): Option[Token] = {
            source.headOption match {
              case Some(x) if x.isLetterOrDigit || x == '_' =>
                b.append(source.next)
                errorTok()
              case Some(x) if x.isWhitespace =>
                None
              case Some(_) =>
                Some(parseBad)
              case None =>
                None
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

        def parseIntLit(b: StringBuffer) : Token = {
          source.headOption match {
            case Some(x) if x.isDigit =>
              b.append(source.next)
              parseIntLit(b)
            case Some(_) | None =>
              new INTLIT(b.toString.toInt)
          }
        }

        def parseEq : Token = {
          source.headOption match {
            case Some(x) if x == '=' =>
              source.next
              new Token(EQUALS)
            case Some(_) | None =>
              new Token(EQSIGN)
          }
        }

        def parseAnd : Token = {
          source.headOption match {
            case Some(x) if x == '&' =>
              source.next
              new Token(AND)
            case Some(_) | None =>
              parseBad
          }
        }

        def parseOr : Token = {
          source.headOption match {
            case Some(x) if x == '|' =>
              source.next
              new Token(OR)
            case Some(_) | None =>
              parseBad
          }
        }

        def parseStr(b : StringBuffer) : Token = {
          source.headOption match {
            case Some(x) if x == '"' =>
              source.next
              new STRLIT(b.toString)
            case Some(_) =>
              b.append(source.next)
              parseStr(b)
            case None =>
              parseBad
          }
        }

        if (!source.hasNext) {
          eof = true
          new Token(EOF)
        } else {
          val currentChar = source.next

          if (currentChar == '/') parseCommentOrDiv(new StringBuffer().append(currentChar))
          else if (currentChar.isLetter) parseIdentOrKeword(new StringBuffer().append(currentChar))
          else if (currentChar.isDigit) parseIntLit(new StringBuffer().append(currentChar))
          else if (currentChar == ':') new Token(COLON)
          else if (currentChar == ';') new Token(SEMICOLON)
          else if (currentChar == '.') new Token(DOT)
          else if (currentChar == ',') new Token(COMMA)
          else if (currentChar == '=') parseEq
          else if (currentChar == '!') new Token(BANG)
          else if (currentChar == '(') new Token(LPAREN)
          else if (currentChar == ')') new Token(RPAREN)
          else if (currentChar == '{') new Token(LBRACE)
          else if (currentChar == '}') new Token(RBRACE)
          else if (currentChar == '&') parseAnd
          else if (currentChar == '|') parseOr
          else if (currentChar == '<') new Token(LESSTHAN)
          else if (currentChar == '+') new Token(PLUS)
          else if (currentChar == '-') new Token(MINUS)
          else if (currentChar == '*') new Token(TIMES)
          else if (currentChar == '"') parseStr(new StringBuffer())
          else if (currentChar.isWhitespace) next
          else parseBad
        }
      }
    }
  }
}
