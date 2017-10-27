package punkt0
package lexer

import java.io.File

object Lexer extends Phase[File, Iterator[Token]] {

  def run(f: File)(ctx: Context): Iterator[Token] = {
    var eof: Boolean = false
    val source = scala.io.Source.fromFile(f) // Needed for pos
    val bufferedSource = source.buffered
    var tokStartPos = 0

    new Iterator[Token] {

      private def peek : Option[Char] = bufferedSource.headOption

      def hasNext: Boolean = !eof

      def next: Token = {

        // Invalid (sequence of) character(s)
        def parseBad(m : String): Token = {
          val tok = new Token(BAD)
          tok.setPos(f, source.pos)
          Reporter.error(m, tok)
          tok
        }

        //  // | /* | /
        def parseCommentOrDiv(b : StringBuffer) : Token = {
          peek match {
            case Some(x) if x == '/' =>
              // Advance until we find a newline
              while ((b.indexOf(scala.util.Properties.lineSeparator) == -1) && bufferedSource.hasNext) {
                b.append(bufferedSource.next)
              }

              // Return the next token
              next
            case Some(x) if x == '*' =>
              // Advance until we find the */
              while (b.indexOf("*/") == -1) {
                if(b.length() > 1 && b.substring(2).indexOf("/*") != -1) return parseBad("nested block comment")
                if(bufferedSource.hasNext) {
                  b.append(bufferedSource.next)
                } else {
                  return parseBad("unexpected EOF in block comment")
                }
              }

              next
            case Some(_) | None =>
              new Token(DIV)
          }
        }

        //  myVar | class ...
        def parseIdentOrKeword(b : StringBuffer) : Token = {
          while(peek.isDefined && peek.get.isLetterOrDigit || peek.get == '_' ) {
            b.append(bufferedSource.next)
          }

          Keywords.lookup(b.toString) match {
            case Some(kw) => new Token(kw)
            case None => new ID(b.toString)
          }
        }

        //  1 | 2 ...
        def parseIntLit(b: StringBuffer) : Token = {
          if (b.charAt(0) == '0') return new INTLIT(b.toString.toInt)
          peek match {
            case Some(x) if x.isDigit =>
              b.append(bufferedSource.next)
              parseIntLit(b)
            case Some(_) | None =>
              new INTLIT(b.toString.toInt)
          }
        }

        //  == | =
        def parseEq : Token = {
          peek match {
            case Some(x) if x == '=' =>
              bufferedSource.next
              new Token(EQUALS)
            case Some(_) | None =>
              new Token(EQSIGN)
          }
        }

        //  &&
        def parseAnd : Token = {
          peek match {
            case Some(x) if x == '&' =>
              bufferedSource.next
              new Token(AND)
            case Some(_) | None =>
              parseBad("unexpected character after '&'")
          }
        }

        // ||
        def parseOr : Token = {
          peek match {
            case Some(x) if x == '|' =>
              bufferedSource.next
              new Token(OR)
            case Some(_) | None =>
              parseBad("unexpected character after '|'")
          }
        }

        //  "Hello World!"
        def parseStr(b : StringBuffer) : Token = {
          peek match {
            case Some(x) if x == '"' =>
              bufferedSource.next
              new STRLIT(b.toString)
            case Some(x) if x == '\n' =>
              bufferedSource.next
              parseBad("unexpected newline in 'string'")
            case Some(_) =>
              b.append(bufferedSource.next)
              parseStr(b)
            case None =>
              parseBad("unexpected EOF in 'string'")
          }
        }

        var resTok = new Token(BAD) // TODO: Find a way to remove this variable

        if (!bufferedSource.hasNext) {
          eof = true
          resTok = new Token(EOF)
        } else {
          val currentChar = bufferedSource.next
          tokStartPos = source.pos

          resTok = {
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
            else parseBad("unexpected character")
          }
        }

        if(resTok.kind != BAD) {
          resTok.setPos(f, tokStartPos)
        } else {
          // The position is already set to the position of the invalid character (not necessarily at the start of the token)
        }
        resTok
      }
    }
  }
}
