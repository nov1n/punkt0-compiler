package punkt0
package analyzer

import ast.Trees._
import Types._
import punkt0.analyzer.Symbols.VariableSymbol
//TODO: Anyref

object TypeChecking extends Phase[Program, Program] {

  val _printTypes: Boolean = false

  def calcLeastUpperBound(t1: Type, t2: Type): Type = (t1, t2) match {
    case (TClass(cs1), TClass(cs2)) if cs1.getType.isSubTypeOf(cs2.getType) => cs2.getType // cs1 < cs2
    case (TClass(cs1), TClass(cs2)) if cs2.getType.isSubTypeOf(cs1.getType) => cs1.getType // cs2 < cs1
    case (TClass(csm1), TClass(cs2)) => // find common superclass, fall back to anyRef
      var cs1 = csm1
      while (!cs1.getType.isSubTypeOf(cs2.getType) && !cs2.getType.isSubTypeOf(cs1.getType)) {
        cs1.parent match {
          case Some(x) => cs1 = x
          case None => return anyRef
        }
      }
      cs1.getType
    case (x, y) if x == y => x
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
          tcExpr(e, TBoolean, TString, TInt)
          TUnit
        case Block(e) =>
          if(e.isEmpty) return TUnit
          e.reverse.tail.map(x => tcExpr(x))
          tcExpr(e.reverse.head, expected : _*) // TODO: Understand what it means to pass along expected
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
          TBoolean
        case Equals(l, r) =>
          tcExpr(l) match {
            case TInt => tcExpr(r, TInt)
            case TBoolean => tcExpr(r,TBoolean)
            case TString => tcExpr(r,TString)
            case TUnit => tcExpr(r,TUnit)
            case TClass(cs) => tcExpr(r, cs.getType)
            case TAnyRef(cs) => tcExpr(r, cs.getType)
            case TError | TUntyped => tcExpr(r)
          }
          TBoolean
        case While(cond, body) =>
          tcExpr(cond, TBoolean)
          tcExpr(body, TUnit)
        case Assign(id, e) =>
          id.getSymbol match {
            case s : VariableSymbol =>
            // TODO: Disallow assignment to anything other than a variable (Maybe already done)

              // get parent
              // if(parent is a method)
              //   check if id.getSymbol is present in arglist
            case _ =>
              Reporter.error(s"Type error: cannot assign to '${id.getType}'.", id)
              return TUnit
          }
          tcExpr(e, id.getType)
          TUnit
        case MethodCall(obj, meth, args) =>
          // Find out what object the method is called on
          val objType = tcExpr(obj)
          obj.setType(objType)

          // Evaluate all arguments
          args.foreach(a => a.setType(tcExpr(a)))

          // Return the return type of the method
          objType match {
            case TClass(cs) => cs.lookupMethod(meth.value) match {
              case Some(x) =>
                // We have found the object on which the method is called

                // Make sure the number of provided arguments match the method
                val expArgs = x.argList.length
                val actArgs = args.length
                if(expArgs != actArgs) {
                  Reporter.error(s"Type error: wrong number of arguments to call method '${x.name}', got $actArgs, expected $expArgs.", meth)
                }

                // Ensure arg types match provided types
                val incorrectArgs = x.argList
                  .zip(args)
                  .filter({case (e, a) => !a.getType.isSubTypeOf(e.getType)})
                if(incorrectArgs.nonEmpty) {
                  Reporter.error(s"Type error: wrong argument types in method '${x.name}', got ${incorrectArgs.map(_._2.getType).mkString(", ")}, expected ${incorrectArgs.map(_._1.getType).mkString(", ")}.", incorrectArgs.head._1)
                }

                // Attach method symbol, we now know the dispatch
                meth.setSymbol(x)

                // Return method return type
                x.getType
              case None =>
                Reporter.error(s"Type error: no method ${meth.value} exists on object of type $objType.", meth)
                TError
            }
            case x =>
              Reporter.error(s"Type error: cannot call methods on type '$x.", obj)
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

      // Debug
      if(_printTypes) println(expr, tpe)

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
