package punkt0
package analyzer

import ast.Trees._
import Symbols._
import Types._
//TODO: Anyref

object TypeChecking extends Phase[Program, Program] {

  def calcLeastUpperBound(t1: Type, t2: Type): Type = (t1, t2) match {
    case (TClass(csm1), TClass(cs2)) =>
      var cs1 = csm1
      while (!cs1.getType.isSubTypeOf(cs2.getType) && !cs2.getType.isSubTypeOf(cs1.getType)) {
        cs1.parent match {
          case Some(x) => cs1 = x
          case None => return anyRef
        }
      }
      cs1.getType
    case (_, _) => anyRef
  }

  /** Typechecking does not produce a value, but has the side effect of
   * attaching types to trees and potentially outputting error messages. */
  def run(prog: Program)(ctx: Context): Program = {
    def tcExpr(expr: ExprTree, expected: Type*): Type = {
      val tpe: Type = expr match {
        case True() => TBoolean
        case False() => TBoolean
        case Null() => TUntyped
        case IntLit(_) => TInt
        case StringLit(_) => TString
        case x @ This() => x.getSymbol.getType
        case x @ Identifier(_) => x.getSymbol.getType
        case New(i) => i.getType
        case Not(e) => tcExpr(e, TBoolean)
        case Println(e) =>
          tcExpr(e)
          TUnit
        case Block(e) =>
          e.reverse.tail.map(x => tcExpr(x))
          tcExpr(e.reverse.head, expected : _*)
        case And(l, r) =>
          tcExpr(l, TBoolean)
          tcExpr(r, TBoolean)
        case Or(l, r) =>
          tcExpr(l, TBoolean)
          tcExpr(r, TBoolean)
        case p @ Plus(l, r) =>
          val lType = tcExpr(l)
           lType match {
            case TInt =>
              tcExpr(r, TInt, TString) match {
                case TInt =>
                  TInt
                case TString =>
                  TString
                case x =>
                  Reporter.error(s"Type error: adding $lType to $x.", p)
                  TError
              }
            case TString =>
              tcExpr(r, TInt, TString) match {
                case TInt | TString =>
                  TString
                case x =>
                  Reporter.error(s"Type error: adding $lType to $x.", p)
                  TError
              }
            case _ =>
              Reporter.error(s"Type error: cannot add to $lType", p)
              TError
          }
        case Minus(l, r) =>
          tcExpr(l, TInt)
          tcExpr(r, TInt)
        case Times(l, r) =>
          tcExpr(l, TInt)
          tcExpr(r, TInt)
        case Div(l, r) =>
          tcExpr(l, TInt)
          tcExpr(r, TInt)
        case LessThan(l, r) =>
          tcExpr(l, TInt)
          tcExpr(r, TInt)
        case Equals(l, r) =>
          tcExpr(l) match {
            case TInt => tcExpr(r, TInt)
            case TBoolean => tcExpr(r,TBoolean)
            case TString => tcExpr(r,TString)
            case TUnit => tcExpr(r,TUnit)
            case TClass(cs) => tcExpr(r, cs.getType)
          }
        case While(cond, body) =>
          tcExpr(cond, TBoolean)
          tcExpr(body, TUnit)
        case Assign(id, e) =>
          tcExpr(e, id.getType)
          TUnit
        case MethodCall(obj, meth, args) =>
          // Find out what object the method is called on
          val objType = tcExpr(obj, expected :_*)
          obj.setType(objType)

          // Evaluate all arguments
          args.foreach(a => tcExpr(a, a.getType))

          // Return the return type of the method
          objType match {
            case TClass(cs) => cs.lookupMethod(meth.value) match {
              case Some(x) =>
                meth.setSymbol(x)
                x.getType
              case None =>
                Reporter.error(s"Type error: no method ${meth.value} exists on object of type $objType.", meth)
                TError
            }
            case x =>
              Reporter.error(s"Type error: object was not a class but $x in method call.")
              TError
          }
        case If(ifexp, thn, els) =>
          tcExpr(ifexp, TBoolean)
          val thnType = tcExpr(thn)
          els match {
            case Some(x) =>
              val elsType = tcExpr(x)
              calcLeastUpperBound(thnType, elsType)
            case None => thnType
          }
      }
      println(expr, tpe)

      // Check result and return a valid type in case of error
      if (expected.isEmpty) {
        tpe
      } else if (!expected.exists(e => tpe.isSubTypeOf(e))) {
        Reporter.error("Type error: expected: " + expected.toList.mkString(" or ") + ", found: " + tpe, expr)
        expected.head
      } else {
        tpe
      }
    }

    def tcVarDecl(vd: VarDecl, expected: Type*): Type = vd match {
      case VarDecl(tpe, _, expr) => tcExpr(expr, typeTreeToTyped(tpe): _*)
    }

    prog.main.vars.foreach(v => tcVarDecl(v))
    prog.main.exprs.foreach(e => tcExpr(e))

    prog.classes.foreach(c => c.vars.foreach(v => tcVarDecl(v)))
    prog.classes.foreach(c => c.methods.foreach(m => {
      m.vars.foreach(v => tcVarDecl(v))
      m.exprs.foreach(e => tcExpr(e))
      tcExpr(m.retExpr, typeTreeToTyped(m.retType): _*)
    }))

    prog
  }

  private def typeTreeToTyped(tpe: TypeTree): List[Type] = {
    tpe match {
      case BooleanType() => List(TBoolean)
      case IntType() => List(TInt)
      case UnitType() => List(TUnit)
      case StringType() => List(TString)
      case t => List(t.getType, TUntyped)
    }
  }
}
