package punkt0
package ast

import Trees._
import lexer._

import scala.util.Try

object Parser extends Phase[Iterator[Token], Program] {
  def run(tokens: Iterator[Token])(ctx: Context): Program = {
    import Reporter._
    /** Store the current token, as read from the lexer. */
    var currentToken: Token = new Token(BAD)

    def readToken: Unit = {
      if (tokens.hasNext) {
        // uses nextToken from the Lexer trait
        currentToken = tokens.next

        // skips bad tokens
        while (currentToken.kind == BAD) {
          currentToken = tokens.next
        }
      }
    }

    /** ''Eats'' the expected token, or terminates with an error. */
    def eat(kind: TokenKind): Unit = {
      if (currentToken.kind == kind) {
        readToken
      } else {
        expected(kind)
      }
    }

    /** Complains that what was found was not expected. The method accepts arbitrarily many arguments of type TokenKind */
    def expected(kind: TokenKind, more: TokenKind*): Nothing = {
      fatal("expected: " + (kind::more.toList).mkString(" or ") + ", found: " + currentToken, currentToken)
    }

    def parseGoal: Program = {
      program
    }

    // program ::=
    def program : Program = {
      // classDeclaration // optional
      Program(mainDeclaration, List()) // required
    }

    // MainDeclaration ::= object Identifier extends Identifier { ( VarDeclaration ) Expression ( ; Expression ) }
    def mainDeclaration : MainDecl = {
      eat(OBJECT)
      val obj = identifier
      eat(EXTENDS)
      val parent = identifier
      eat(LBRACE)

      var vars = List[VarDecl]()
      var exprs = List[ExprTree]()

      // 0 or more VarDeclarations
      while (currentToken.kind == VAR) {
        vars :+= varDeclaration
      }

      // Exactly 1 Expression
      exprs :+= expression

      // 0 or more ;Expression
      while (currentToken.kind == SEMICOLON) {
        readToken // advance over ;
        exprs :+= expression
      }

      eat(RBRACE)
      MainDecl(obj, parent, vars, exprs)
    }

    // Identifier	::=	<IDENTIFIER>
    def identifier : Identifier = {
      val idOpt = Try(currentToken.asInstanceOf[ID]).toOption
      readToken
      idOpt match {
        case Some(x) => Identifier(x.value)
        case None => expected(IDKIND)
      }
    }

    // VarDeclaration ::= var Identifier : Type = Expression ;
    def varDeclaration : VarDecl = {
      eat(VAR)
      val id = identifier
      eat(COLON)
      val tpe : TypeTree = currentToken.kind match {
        case BOOLEAN => readToken; BooleanType()
        case INT => readToken; IntType()
        case STRING => readToken; StringType()
        case UNIT => readToken; UnitType()
        case _ => identifier
      }
      eat(EQSIGN)
      val expr = expression
      eat(SEMICOLON)
      VarDecl(tpe, id, expr)
    }

    // Expression ::=
    def expression : ExprTree = {
      if (currentToken.kind == PRINTLN) {
        readToken // Advance over Println
        eat(LPAREN) // Advance over (
        val t = expression // Recursion handles advancing lexer
        eat(RPAREN)
        Println(t)
      } else if(currentToken.kind == STRLITKIND) {
        val strLit = Try(currentToken.asInstanceOf[STRLIT]).toOption
        readToken
        strLit match {
          case Some(x) => StringLit(x.value)
          case None => expected(STRLITKIND)
        }
      } else {
        readToken
        This()
      }
    }

    readToken
    val tree = parseGoal
    terminateIfErrors
    tree
  }
}
