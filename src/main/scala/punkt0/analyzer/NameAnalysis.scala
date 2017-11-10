package punkt0
package analyzer

import ast.Trees._
import Symbols._
import punkt0.ast.Printer

object NameAnalysis extends Phase[Program, Program] {

  val globalScope = new GlobalScope()

  def run(prog: Program)(ctx: Context): Program = {
    import Reporter._

    // Step 1: Collect symbols in declarations
    collectSymbols(prog)
//    printSummary()

    // Step 1.5: Create the class hierarchy
    addClassHierarchy(prog)

    // Step 2: Attach symbols to identifiers (except method calls) in method bodies
    symbolizeIdentifiers(prog)

    // Make sure you check all constraints

    prog
  }

  private def addClassHierarchy(prog: Program): Unit = {
    prog.classes.foreach(c => c.parent match { // TODO: Understand this
      case Some(parentTree) =>
        val parentSymbolOption = globalScope.lookupClass(parentTree.value)

        // Add symbol to parent identifier for printing
        parentSymbolOption match {
          case None => Reporter.error("Extending unexisting class: " + parentTree.value)
          case Some(parentSymbol) => {
            parentTree.setSymbol(parentSymbol)
          }
        }

        // Create a link between parent and children symbols for variable lookups
        c.getSymbol.parent = parentSymbolOption
      case None => Unit
    })
  }

  private def collectSymbols(prog: Program): Unit = {
    // Main
    val ms = new ClassSymbol(prog.main.obj.value).setPos(prog.main)
    prog.main.setSymbol(ms)
    globalScope.mainClass = ms
    prog.main.vars.foreach(v => {
      val variableSymbol = new VariableSymbol(v.id.value).setPos(v)
      v.setSymbol(variableSymbol)
      prog.main.getSymbol.members += variableSymbol.name -> variableSymbol
    })

    // Classes
    prog.classes.foreach(c => {
      val className = c.id.value
      val classSymbol = new ClassSymbol(className).setPos(c)

      // Classes
      c.setSymbol(classSymbol)
      globalScope.classes += className -> classSymbol

      // Variables
      c.vars.foreach(v => {
        val variableName = v.id.value
        val variableSymbol = new VariableSymbol(variableName).setPos(v)
        v.setSymbol(variableSymbol)
        c.getSymbol.members += variableName -> variableSymbol
      })

      // Methods
      c.methods.foreach(m => {
        // Methods
        val methodName = m.id.value
        val methodSymbol = new MethodSymbol(methodName, classSymbol).setPos(m)
        m.setSymbol(methodSymbol)
        c.getSymbol.methods += methodName -> methodSymbol

        // Arguments
        m.args.foreach(a => {
          val variableName = a.id.value
          val variableSymbol = new VariableSymbol(variableName).setPos(a)
          a.setSymbol(variableSymbol)
          m.getSymbol.argList = m.getSymbol.argList :+ variableSymbol
        })

        // Variables
        m.vars.foreach(v => {
          val variableName = v.id.value
          val variableSymbol = new VariableSymbol(variableName).setPos(v)
          v.setSymbol(variableSymbol)
          m.getSymbol.members += variableName -> variableSymbol
        })
      })
    })
  }

  def symbolizeIdentifiers(p: Program): Unit = {
    symbolizeIdentifiers(p.main, p.main)
    p.classes.foreach(c => symbolizeIdentifiers(c, c))
  }

  def symbolizeIdentifiers(tree: Tree, scope : Symbolic[_]) : Unit = {
    def binaryTraverse(lhs: Tree, rhs: ExprTree): Unit = {
      symbolizeIdentifiers(lhs, scope)
      symbolizeIdentifiers(rhs, scope)
    }

    tree match {
      case And(lhs, rhs) => binaryTraverse(lhs, rhs)
      case Or(lhs, rhs) => binaryTraverse(lhs, rhs)
      case Plus(lhs, rhs) => binaryTraverse(lhs, rhs)
      case Minus(lhs, rhs) => binaryTraverse(lhs, rhs)
      case Times(lhs, rhs) => binaryTraverse(lhs, rhs)
      case Div(lhs, rhs) => binaryTraverse(lhs, rhs)
      case LessThan(lhs, rhs) => binaryTraverse(lhs, rhs)
      case Equals(lhs, rhs) => binaryTraverse(lhs, rhs)
      case VarDecl(tpe, id, expr) =>
        symbolizeIdentifiers(tpe, scope)
        symbolizeIdentifiers(expr, scope)
      case While(cond, body) => binaryTraverse(cond, body)
      case MainDecl(obj, parent, vars, exprs) =>
        vars.foreach(symbolizeIdentifiers(_, scope))
        exprs.foreach(symbolizeIdentifiers(_, scope))
      case ClassDecl(id, parent, vars, methods) =>
        vars.foreach(symbolizeIdentifiers(_, scope))
        methods.foreach(m => symbolizeIdentifiers(m, m))
      case MethodDecl(overrides, retType, id, args, vars, exprs, retExpr) =>
        symbolizeIdentifiers(retType, scope)
        args.foreach(symbolizeIdentifiers(_, scope))
        vars.foreach(symbolizeIdentifiers(_, scope))
        exprs.foreach(symbolizeIdentifiers(_, scope))
        symbolizeIdentifiers(retExpr, scope)
      case MethodCall(obj, meth, args) =>
        symbolizeIdentifiers(obj, scope)
        meth.setSymbol(new VariableSymbol(meth.value)) // We don't know the dispatch at this point
        args.foreach(a => symbolizeIdentifiers(a, scope))
      case Assign(id, expr) =>
        symbolizeIdentifier(id, scope, canBeGlobal = false)
        symbolizeIdentifiers(expr, scope)
      case id @ Identifier(_) => symbolizeIdentifier(id, scope, canBeGlobal = true)
      case New(tpe) => symbolizeIdentifier(tpe, scope, canBeGlobal = true)
      case Not(expr) => symbolizeIdentifiers(expr, scope)
      case Formal(tpe, id) =>
        symbolizeIdentifiers(tpe, scope)
        symbolizeIdentifier(id, scope, canBeGlobal = false)
      case Block(exprs) => exprs.foreach(e => symbolizeIdentifiers(e, scope))
      case Println(expr) => symbolizeIdentifiers(expr, scope)
      case If(expr, thn, els) =>
        binaryTraverse(expr, thn)
        if(els.isDefined) symbolizeIdentifiers(els.get, scope)
      case IntType() | StringType() | BooleanType() | UnitType() | IntLit(_) | StringLit(_) | False() | True() | This() | Null() => Unit
    }
  }

  def symbolizeIdentifier(id : Identifier, scope: Symbolic[_], canBeGlobal : Boolean) : Unit = { // TODO: Check all 'canBeGlobal' occurrences according to grammar
    scope.getSymbol.asInstanceOf[Symbol].lookupVar(id.value) match {
      case Some(x) => id.setSymbol(x)
      case None if canBeGlobal => globalScope.lookupClass(id.value) match {
        case Some(x) => id.setSymbol(x)
        case None =>Reporter.error(s"Identifier ${id.value} on ${id.posString} not defined")
      }
      case None => throw new RuntimeException("Variable " + id.value + " not found " + id.posString)
    }
  }

  def printSummary(): Any = {
    println("---------- SUMMARY ------------")
    println("MAIN CLASS:")
    println(globalScope.mainClass)
    println()
    println("GLOBAL CLASSES:")
    globalScope.classes.foreach(x => {
      println(x._2 + " extends " + x._2.parent)
      x._2.members.foreach(y => {
        println("\t" + y._2)
      })
      println()
      x._2.methods.foreach(y => {
        println("\t" + y._2 + " overrides " + y._2.overridden + " (" + y._2.argList.mkString(", ") + ")")
        y._2.members.foreach(z => {
          println("\t\t" + z._2)
        })
      })
      println()
    })
    println("----------- END -------------")
    println()
  }
}
