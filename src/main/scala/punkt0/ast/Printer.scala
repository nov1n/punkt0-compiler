package punkt0
package ast

import Trees._
import analyzer.Symbols._
import lexer._

object Printer {
  private var _printSymbols = false

  val keywords = Map[TokenKind, String](
    COLON -> ":",
    SEMICOLON -> ";",
    DOT -> ".",
    COMMA -> ",",
    EQSIGN -> "=",
    EQUALS -> "==",
    BANG -> "!",
    LPAREN -> "(",
    RPAREN -> ")",
    LBRACE -> "{",
    RBRACE -> "}",
    AND -> "&&",
    OR -> "||",
    LESSTHAN -> "<",
    PLUS -> "+",
    MINUS -> "-",
    TIMES -> "*",
    DIV -> "/",
    OBJECT -> "object",
    CLASS -> "class",
    DEF -> "def",
    OVERRIDE -> "override",
    VAR -> "var",
    UNIT -> "Unit",
    STRING -> "String",
    EXTENDS -> "extends",
    INT -> "Int",
    BOOLEAN -> "Boolean",
    WHILE -> "while",
    IF -> "if",
    ELSE -> "else",
    TRUE -> "true",
    FALSE -> "false",
    THIS -> "this",
    NULL -> "null",
    NEW -> "new",
    PRINTLN -> "println"
  )

  def apply(t: Tree, printSymbols: Boolean): String = {
    _printSymbols = printSymbols
    apply(new StringBuilder, 0, t).toString()
  }

  private def apply(sIn : StringBuilder, l : Int, t : Tree): StringBuilder = {
    var s = sIn
    t match {
      case Program(main, classes) =>
        classes.foreach(c => {
          s = apply(s, l, c)
        })
        s = apply(s, l, main)
      case m @ MainDecl(obj, parent, vars, exprs) =>
        s.append(s"${keywords(OBJECT)} ${valOrSymbol(m)} ${keywords(EXTENDS)} ${parent.value} ${keywords(LBRACE)}\n")

        vars.foreach(v => {
          s.append("\t" * (l+1))
          s = apply(s, l, v)
          s.append(s"${keywords(SEMICOLON)}")
          s.append("\n")
        })

        exprs.zipWithIndex.foreach({ case (e, i) =>
          s.append("\t" * (l+1))
          s = apply(s, l, e)
          if (i != exprs.size - 1) s.append(s"${keywords(SEMICOLON)}")
          s.append("\n")
        })

        s.append(s"${keywords(RBRACE)}")
        s.append("\n")
      case v @ VarDecl(tpe, id, expr) =>
        s.append(s"${keywords(VAR)} ${valOrSymbol(v)} ${keywords(COLON)} ${valOrSymbol(tpe)} ${keywords(EQSIGN)} ")
        s = apply(s, l, expr)
      case IntLit(value) =>
        s.append(value)
      case StringLit(value) =>
        s.append("\"")
        s.append(s"$value")
        s.append("\"")
      case Println(expr) =>
        s.append(s"${keywords(PRINTLN)}${keywords(LPAREN)}")
        s = apply(s, l, expr)
        s.append(s"${keywords(RPAREN)}")
      case c @ ClassDecl(id, parent, vars, methods) =>
        val ext = parent match {
          case Some(p) => s"${keywords(EXTENDS)} ${valOrSymbol(p)} "
          case None => ""
        }
        s.append(s"${keywords(CLASS)} ${valOrSymbol(c)} $ext${keywords(LBRACE)}\n")

        vars.zipWithIndex.foreach({case(v, i) =>
          s.append("\t" * (l+1))
          s = apply(s, l, v)
          s.append(s"${keywords(SEMICOLON)}")
          s.append("\n")
          if (i == vars.size - 1) s.append("\n")
        })

        methods.zipWithIndex.foreach({ case (m, i) =>
          s.append("\t" * (l + 1))
          s = apply(s, l, m)
          if (i != methods.size - 1) s.append("\n\n")
        })

        s.append("\n}\n\n")
      case m @ MethodDecl(overrides, retType, id, args, vars, exprs, retExpr) =>
        val ovrr = if(overrides) s"${keywords(OVERRIDE)} " else ""
        s.append(s"$ovrr${keywords(DEF)} ${valOrSymbol(m)}${keywords(LPAREN)}")

        args.zipWithIndex.foreach({case (a, i) =>
          s.append(s"${valOrSymbol(a)} ${keywords(COLON)} ${valOrSymbol(a.tpe)}")
          if (i != args.size - 1) s.append(s"${keywords(COMMA)} ")
        })

        s.append(s"${keywords(RPAREN)} ${keywords(COLON)} ${valOrSymbol(retType)} ${keywords(EQSIGN)} ${keywords(LBRACE)}\n")

        vars.zipWithIndex.foreach({case (v, i) =>
          s.append("\t" * (l+2))
          s = apply(s, l, v)
          s.append(s"${keywords(SEMICOLON)}")
          s.append("\n")
          if (i == vars.size - 1) s.append("\n")
        })

        exprs.zipWithIndex.foreach({ case (e, i) =>
          s.append("\t" * (l+2))
          s = apply(s, l+1, e)
          s.append(s"${keywords(SEMICOLON)}")
          s.append("\n")
        })

        s.append("\t" * (l+2))
        s = apply(s, l, retExpr)
        s.append("\n")

        s.append("\t" * (l+1))
        s.append("}")
      case id @ Identifier(_) =>
        s.append(valOrSymbol(id))
      case Plus(lhs, rhs) =>
        s = apply(s, l, lhs)
        s.append(s" ${keywords(PLUS)} ")
        s = apply(s, l, rhs)
      case Minus(lhs, rhs) =>
        s = apply(s, l, lhs)
        s.append(s" ${keywords(MINUS)} ")
        s = apply(s, l, rhs)
      case And(lhs, rhs) =>
        s = apply(s, l, lhs)
        s.append(s" ${keywords(AND)} ")
        s = apply(s, l, rhs)
      case Or(lhs, rhs) =>
        s = apply(s, l, lhs)
        s.append(s" ${keywords(OR)} ")
        s = apply(s, l, rhs)
      case Times(lhs, rhs) =>
        s = apply(s, l, lhs)
        s.append(s" ${keywords(TIMES)} ")
        s = apply(s, l, rhs)
      case Div(lhs, rhs) =>
        s = apply(s, l, lhs)
        s.append(s" ${keywords(DIV)} ")
        s = apply(s, l, rhs)
      case LessThan(lhs, rhs) =>
        s = apply(s, l, lhs)
        s.append(s" ${keywords(LESSTHAN)} ")
        s = apply(s, l, rhs)
      case Equals(lhs, rhs) =>
        s = apply(s, l, lhs)
        s.append(s" ${keywords(EQUALS)} ")
        s = apply(s, l, rhs)
      case True() =>
        s.append(s"${keywords(TRUE)}")
      case False() =>
        s.append(s"${keywords(FALSE)}")
      case This() =>
        s.append(s"${keywords(THIS)}")
      case Null() =>
        s.append(s"${keywords(NULL)}")
      case New(id) =>
        s.append(valOrSymbol(id))
      case Not(expr) =>
        s = apply(s, l, expr)
      case Assign(id, expr) =>
        s.append(s"${valOrSymbol(id)} ${keywords(EQSIGN)} ")
        s = apply(s, l, expr)
      case Block(exprs) =>
        s.append(s"${keywords(LBRACE)}\n")
        exprs.zipWithIndex.foreach({case (e, i) =>
            s.append("\t" * (l+2))
            s = apply(s, l, e)
            if (i != exprs.size - 1) s.append(s"${keywords(SEMICOLON)}")
            s.append("\n")
        })
        s.append("\t" * (l+1))
        s.append(s"${keywords(RBRACE)}")
      case If(expr, thn, els) =>
        s.append(s"${keywords(IF)}${keywords(LPAREN)}")
        s = apply(s, l, expr)
        s.append(s"${keywords(RPAREN)} ")
        s = apply(s, l, thn)
        els match {
          case Some(value) =>
            s.append(s" ${keywords(ELSE)} ")
            s = apply(s, l, value)
          case None =>
        }
      case While(cond, body) =>
        s.append(s"${keywords(WHILE)}${keywords(LPAREN)}")
        s = apply(s, l, cond)
        s.append(s"${keywords(RPAREN)} ")
        s = apply(s, l, body)
      case MethodCall(obj, meth, args) =>
        s = apply(s, l, obj)
        s.append(s"${keywords(DOT)}${valOrSymbol(meth)}${keywords(LPAREN)}")
        args.zipWithIndex.foreach({case (a, i) =>
          s = apply(s, l, a)
          if (i != args.size - 1) s.append(s"${keywords(COMMA)} ")
        })
        s.append(s"${keywords(RPAREN)}")
      case x => Reporter.error("Unexpected TreeNode: " + x)
    }
    s
  }

  def valOrSymbol(node: Tree) : String = node match {
    case x: Symbolic[Symbol] if _printSymbols => x.getSymbol.toString
    case UnitType() => keywords(UNIT)
    case StringType() => keywords(STRING)
    case IntType() => keywords(INT)
    case BooleanType() => keywords(BOOLEAN)
    case Identifier(id) => id
    case x => sys.error("Unexpected symbol: " + x)
  }
}
