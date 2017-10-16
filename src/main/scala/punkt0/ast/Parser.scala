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

    // Program ::= ( ClassDeclaration ) MainDeclaration*
    def program : Program = {
      // TODO: Add positions
      var classes = List[ClassDecl]()
      while (currentToken.kind == CLASS) {
        classes :+= classDecl
      }
      Program(mainDeclaration, classes)
    }

    // ClassDecl ::= class Identifier ( extends Identifier )? { ( VarDeclaration ) ( MethodDeclaration ) }
    def classDecl : ClassDecl = {
      eat(CLASS)
      val id = identifier
      var parent : Option[Identifier] = None
      if(currentToken.kind == EXTENDS) {
        eat(EXTENDS)
        parent = Some(identifier)
      }
      eat(LBRACE)

      var vars = List[VarDecl]()
      var meths = List[MethodDecl]()

      while (currentToken.kind == VAR) {
        vars :+= varDeclaration
      }

      while (currentToken.kind == DEF || currentToken.kind == OVERRIDE) {
        meths :+= methodDecl
      }
      eat(RBRACE)
      ClassDecl(id, parent, vars, meths)
    }

    // MainDeclaration ::= object Identifier extends Identifier { ( VarDeclaration ) Expression ( ; Expression ) }
    def mainDeclaration : MainDecl = {
      eat(OBJECT)
      val id = identifier
      eat(EXTENDS)
      val parent = identifier
      eat(LBRACE)

      var vars = List[VarDecl]()
      var exprs = List[ExprTree]()

      while (currentToken.kind == VAR) {
        vars :+= varDeclaration
      }

      exprs :+= expression

      while (currentToken.kind == SEMICOLON) {
        eat(SEMICOLON)
        exprs :+= expression
      }

      eat(RBRACE)
      MainDecl(id, parent, vars, exprs)
    }

    // VarDeclaration ::= var Identifier : Type = Expression ;
    def varDeclaration : VarDecl = {
      eat(VAR)
      val id = identifier
      eat(COLON)
      val tpe = tipe
      eat(EQSIGN)
      val expr = expression
      eat(SEMICOLON)
      VarDecl(tpe, id, expr)
    }

    // MethodDecl ::= ( override )? def Identifier ( ( Identifier : Type ( , Identifier : Type ) )? ) : Type = { ( VarDeclaration ) Expression ( ; Expression ) *}
    def methodDecl : MethodDecl = {
      var overide = false
      if (currentToken.kind == OVERRIDE) {
        overide = true
        eat(OVERRIDE)
      }
      eat(DEF)
      val id = identifier
      eat(LPAREN)
      var args = List[Formal]()
      while (currentToken.kind != RPAREN) { // Parse arguments
        if (currentToken.kind == COMMA) eat(COMMA)
        val aid = identifier
        eat(COLON)
        val atype = tipe
        args :+= Formal(atype, aid)
      }
      eat(RPAREN)
      eat(COLON)
      val ret = tipe
      eat(EQSIGN)
      eat(LBRACE)

      var vars = List[VarDecl]()
      var exprs = List[ExprTree]()

      while (currentToken.kind == VAR) {
        vars :+= varDeclaration
      }

      exprs :+= expression

      while (currentToken.kind == SEMICOLON) {
        eat(SEMICOLON)
        exprs :+= expression
      }

      eat(RBRACE)
      MethodDecl(overide, ret, id, args, vars, exprs.dropRight(1), exprs.last)
    }

    // Identifier	::=	<IDENTIFIER>
    def identifier : Identifier = {
      val idOpt = Try(currentToken.asInstanceOf[ID]).toOption
      eat(IDKIND)
      idOpt match {
        case Some(x) => Identifier(x.value)
        case None => expected(IDKIND)
      }
    }

    // Type := Boolean|Int|String|Unit|Identifier
    def tipe : TypeTree = currentToken.kind match {
        case BOOLEAN => eat(BOOLEAN); BooleanType()
        case INT => eat(INT); IntType()
        case STRING => eat(STRING); StringType()
        case UNIT => eat(UNIT); UnitType()
        case _ => identifier
    }

    // Expression ::=
    def expression : ExprTree = currentToken.kind match {
      // TODO: case binary operators
      // TODO: case method calls
      case INTLITKIND =>
        val intLit = currentToken.asInstanceOf[INTLIT]
        eat(INTLITKIND)
        IntLit(intLit.value)
      case STRLITKIND =>
        val strLit = currentToken.asInstanceOf[STRLIT]
        eat(STRLITKIND)
        StringLit(strLit.value)
      case TRUE =>
        eat(TRUE)
        True()
      case FALSE =>
        eat(FALSE)
        False()
      case IDKIND =>
        val id = identifier
        if (currentToken.kind == EQSIGN) {
          eat(EQSIGN)
          val expr = expression
          Assign(id, expr)
        } else {
          id
        }
      case THIS =>
        eat(THIS)
        This()
      case NULL =>
        eat(NULL)
        Null()
      case NEW =>
        eat(NEW)
        val id = identifier
        eat(LPAREN)
        eat(RPAREN)
        New(id)
      case BANG =>
        eat(BANG)
        val expr = expression
        Not(expr)
      case LPAREN =>
        eat(LPAREN)
        val expr = expression
        eat(RPAREN)
        expr
      case LBRACE =>
        eat(LBRACE)
        var exprs = List[ExprTree]()

        exprs :+= expression
        while (currentToken.kind == SEMICOLON) {
          eat(SEMICOLON)
          exprs :+= expression
        }
        eat(RBRACE)
        Block(exprs)
      case IF =>
        eat(IF)
        eat(LPAREN)
        val cond = expression
        eat(RPAREN)
        val ifBranch = expression
        var elseBranch : Option[ExprTree] = None
        if (currentToken.kind == ELSE) {
          eat(ELSE)
          val elseExpr = expression
          elseBranch = Some(elseExpr)
        }
        If(cond,ifBranch, elseBranch)
      case WHILE =>
        eat(WHILE)
        eat(LPAREN)
        val cond = expression
        eat(RPAREN)
        val body = expression
        While(cond, body)
      case PRINTLN =>
        eat(PRINTLN)
        eat(LPAREN) // Advance over (
        val t = expression // Recursion handles advancing lexer
        eat(RPAREN)
        Println(t)
      case x =>
        throw new RuntimeException("Invalid token" + x)
    }

    readToken
    val tree = parseGoal
    terminateIfErrors()
    tree
  }
}
