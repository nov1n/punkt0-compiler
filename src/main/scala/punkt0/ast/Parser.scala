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

    def parseGoal: Program = program

    /** Program ::= ( ClassDecl ) MainDecl */
    def program : Program = {
      var classes = List[ClassDecl]()
      while (currentToken.kind == CLASS) {
        classes :+= classDecl
      }
      Program(mainDeclaration, classes)
    }

    /** ClassDecl ::= class Identifier ( extends Identifier )? { ( VarDecl ) ( MethodDecl ) } */
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

    /** MainDecl ::= object Identifier extends Identifier { ( VarDecl ) Expression ( ; Expression ) } */
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

    /** VarDecl ::= var Identifier : Tipe = Expression ; */
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

    /** ( override )? def Identifier ( ( Identifier : Tipe ( , Identifier : Tipe ) )? ) : Tipe = { ( VarDecl ) Expression ( ; Expression ) *} */
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
      while (currentToken.kind != RPAREN) {
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

    /** Identifier ::= <\IDENTIFIER/> */
    def identifier : Identifier = {
      val idOpt = Try(currentToken.asInstanceOf[ID]).toOption
      eat(IDKIND)
      idOpt match {
        case Some(x) => Identifier(x.value)
        case None => expected(IDKIND)
      }
    }

    /** Tipe ::= Boolean
               | Int
               | String
               | Unit
               | Identifier */
    def tipe : TypeTree = currentToken.kind match {
      case BOOLEAN => eat(BOOLEAN); BooleanType()
      case INT => eat(INT); IntType()
      case STRING => eat(STRING); StringType()
      case UNIT => eat(UNIT); UnitType()
      case _ => identifier
    }

    /** Expression ::= Disjunction
      *
      * grammar for expressions is in the order of operator precedence
      * */
    def expression : ExprTree = disjunction

    /** Expression ::= Conjunction ('||' Conjunction)*
      *
      * left associative
      * */
    def disjunction : ExprTree = {
      var res = conjunction
      while (currentToken.kind == OR) {
          eat(OR)
          res = Or(res, conjunction)
      }
      res
    }

    /** Conjunction ::= Equalities ('&&' Equalities)*
      *
      * left associative
      * */
    def conjunction : ExprTree = {
      var res = equalities
      while (currentToken.kind == AND) {
        eat(AND)
        res = And(res, equalities)
      }
      res
    }

    /** Equalities ::= AdditionSubtraction ( '<=' | '==' AdditionSubtraction )*
      *
      * left associative
      * */
    def equalities : ExprTree = {
      var res = additionSubtraction
      while ( currentToken.kind == LESSTHAN || currentToken.kind == EQUALS) {
        if (currentToken.kind == LESSTHAN) {
          eat(LESSTHAN)
          res = LessThan(res, additionSubtraction)
        } else if (currentToken.kind == EQUALS) {
          eat(EQUALS)
          res = Equals(res, additionSubtraction)
        }
      }
      res
    }

    /** AdditionSubtraction ::= DivTimes ( '+' | '-'  DivTimes)*
      *
      * left associative
      * */
    def additionSubtraction : ExprTree = {
      var res = divTimes
      while ( currentToken.kind == PLUS || currentToken.kind == MINUS) {
        if (currentToken.kind == PLUS) {
          eat(PLUS)
          res = Plus(res, divTimes)
        } else if (currentToken.kind == MINUS) {
          eat(MINUS)
          res = Minus(res, divTimes)
        }
      }
      res
    }

    /** DivTimes ::= TermMethod ( '/' | '*'  termMethod)*
      *
      * left associative
      * */
    def divTimes : ExprTree = {
      var res = termMethod
      while ( currentToken.kind == DIV || currentToken.kind == TIMES) {
        if (currentToken.kind == DIV) {
          eat(DIV)
          res = Div(res, termMethod)
        } else if (currentToken.kind == TIMES) {
          eat(TIMES)
          res = Times(res, termMethod)
        }
      }
      res
    }

    /** TermMethod ::= Term ('.'  Identifier '(' Expression (',' Expression)* ')' )*
      *
      * left associative
      * */
    def termMethod : ExprTree = {
      var obj = term
      while (currentToken.kind == DOT) {
        eat(DOT)
        val meth = identifier
        eat(LPAREN)
        var args = List[ExprTree]()
        while (currentToken.kind != RPAREN) {
          if (currentToken.kind == COMMA) eat(COMMA)
          args :+= expression
        }
        eat(RPAREN)
        obj = MethodCall(obj, meth, args)
      }
      obj
    }


    /** Term ::= <\INTEGER_LITERAL/>
               | <\STRING_LITERAL/>
               | true
               | false
               | Identifier ('=' Expression)?
               | this
               | null
               | new Identifier '()'
               | ! Expression
               | ( Expression )
               | { ( Expression ( ; Expression ) )? *}
               | if ( Expression ) Expression ( else Expression )?
               | while ( Expression ) Expression
               | println ( Expression )
      * */
    def term : ExprTree = {
      val res = currentToken.kind match {
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
          val expr = termMethod // Binds tightest
          Not(expr)
        case LPAREN =>
          eat(LPAREN)
          val expr = expression
          eat(RPAREN)
          expr
        case LBRACE =>
          eat(LBRACE)
          var exprs = List[ExprTree]()
          if (currentToken.kind != RBRACE) {
            exprs :+= expression
            while (currentToken.kind == SEMICOLON) {
              eat(SEMICOLON)
              exprs :+= expression
            }
          }
          eat(RBRACE)
          Block(exprs)
        case IF =>
          eat(IF)
          eat(LPAREN)
          val cond = expression
          eat(RPAREN)
          val ifBranch = expression
          var elseBranch: Option[ExprTree] = None
          if (currentToken.kind == ELSE) {
            eat(ELSE)
            val elseExpr = expression
            elseBranch = Some(elseExpr)
          }
          If(cond, ifBranch, elseBranch)
        case WHILE =>
          eat(WHILE)
          eat(LPAREN)
          val cond = expression
          eat(RPAREN)
          val body = expression
          While(cond, body)
        case PRINTLN =>
          eat(PRINTLN)
          eat(LPAREN)
          val t = expression
          eat(RPAREN)
          Println(t)
        case x =>
          Reporter.error("invalid token: " + x)
          Null() // TODO: What to do here?
      }
      res
    }

    readToken
    val tree = parseGoal
    terminateIfErrors()
    tree
  }
}
