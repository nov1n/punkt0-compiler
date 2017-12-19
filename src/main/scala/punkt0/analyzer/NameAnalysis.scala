package punkt0
package analyzer

import ast.Trees._
import Symbols._
import punkt0.analyzer.Types._

object NameAnalysis extends Phase[Program, Program] {

  var globalScope = new GlobalScope()

  def run(prog: Program)(ctx: Context): Program = {
    // Step 0: Add hardcoded class names to the global scope
    addHardcodedSymbols()

    // Step 1: Collect symbols in declarations
    collectSymbols(prog)

    // Step 1.5: Create the class hierarchy
    addClassHierarchy(prog)

    // Prevent inheritance cycles
    Enforce.irreflexiveTransitiveClosure(prog.classes)

    // Step 2: Attach symbols to identifiers (except method calls) in method bodies
    symbolizeIdentifiers(prog)

    // Step 3: Add types to symbols
    attachTypes(prog)

    // Step 4: Enforce type-based constraints
    prog.classes.foreach(c => c.methods.foreach(Enforce.methodConstraints))

    if(ctx.debug) debug()

    prog
  }

  private def addHardcodedSymbols(): Unit = {
    globalScope.classes += anyRef.classSymbol.name -> anyRef.classSymbol
    globalScope.classes += appRef.classSymbol.name -> appRef.classSymbol
  }

  private def addClassHierarchy(prog: Program): Unit = {
    prog.classes.foreach(c => c.parent match {
      case Some(parentTree) =>
        val parentSymbolOption = globalScope.lookupClass(parentTree.value)

        // Enforce parent class exists
        parentSymbolOption match {
          case Some(parentSymbol) => parentTree.setSymbol(parentSymbol)
          case None => Reporter.error("extending unexisting class: " + parentTree.value, c)
        }

        // Create a link between parent and children symbols for variable lookups
        c.getSymbol.parent = parentSymbolOption
      case None => Unit
    })
  }

  def attachType(s : Symbol, tree : Tree): Unit = {
    def typeTreeToTyped(t : Tree): Type = t match {
      case BooleanType() => TBoolean
      case IntType() => TInt
      case UnitType() => TUnit
      case StringType() => TString
      case Identifier(x) => globalScope.lookupClass(x) match {
        case Some(c) => TClass(c)
        // Add this so that we can continue with the phase, will never be used because an error has been reported previously
        case None => TClass(new ClassSymbol(""))
      }
    }

    val resType = s match {
      case symbol : ClassSymbol if symbol.name == "App" => appRef
      case symbol: ClassSymbol => TClass(symbol)
      case _ : VariableSymbol => tree match {
        case f: Formal => typeTreeToTyped(f.tpe)
        case v: VarDecl => typeTreeToTyped(v.tpe)
      }
      case _ : MethodSymbol => typeTreeToTyped(tree.asInstanceOf[MethodDecl].retType)
    }
    s.setType(resType)
  }

  private def attachTypes(prog: Program) : Unit = {
    // Main
    attachType(prog.main.parent.getSymbol, prog.main.parent)
    attachType(prog.main.getSymbol, prog.main)
    prog.main.vars.foreach(v => {
      attachType(v.getSymbol, v)
    })

    // Classes
    prog.classes.foreach(c => {
      attachType(c.getSymbol, c)
    })

    // Variables
    // Methods
    prog.classes.foreach(c => {
      // Variables
      c.vars.foreach(v => {
        attachType(v.getSymbol, v)
      })

      // Methods
      c.methods.foreach(m => {
        // Methods
        attachType(m.getSymbol, m)

        // Arguments
        m.args.foreach(a => {
          attachType(a.getSymbol, a)
        })

        // Variables
        m.vars.foreach(v => {
          attachType(v.getSymbol, v)
        })
      })
    })
  }

  private def collectSymbols(prog: Program): Unit = {
    prog.main.parent.setSymbol(new ClassSymbol(prog.main.parent.value))

    // Ensure the parent is 'App'
    if(prog.main.parent.value != appRef.classSymbol.name) {
      Reporter.error(s"Main object should extend 'App'", prog.main)
    }

    // Main
    val ms = new ClassSymbol(prog.main.obj.value).setPos(prog.main)
    prog.main.setSymbol(ms)
    prog.main.obj.setSymbol(ms)
    globalScope.mainClass = ms
    prog.main.vars.foreach(v => {
      Enforce.varUniqueInScope(v, prog.main)
      val variableSymbol = new VariableSymbol(v.id.value).setPos(v)
      v.setSymbol(variableSymbol)
      v.id.setSymbol(variableSymbol)
      prog.main.getSymbol.members += variableSymbol.name -> variableSymbol
    })

    // Classes
    prog.classes.foreach(c => {
      val className = c.id.value
      val classSymbol = new ClassSymbol(className).setPos(c)
      classSymbol.foreignPath = c.foreignPath

      // Classes
      c.setSymbol(classSymbol)
      c.id.setSymbol(classSymbol)
      Enforce.classUnique(c, globalScope.classes)
      globalScope.classes += className -> classSymbol
    })

    // Variables
    // Methods
    prog.classes.foreach(c => {
      // Variables
      c.vars.foreach(v => {
        val variableName = v.id.value
        Enforce.varUniqueInScope(v, c)
        val variableSymbol = new VariableSymbol(variableName).setPos(v)
        v.setSymbol(variableSymbol)
        v.id.setSymbol(variableSymbol)
        c.getSymbol.members += variableName -> variableSymbol
      })

      // Methods
      c.methods.foreach(m => {
        // Methods
        val methodName = m.id.value
        val methodSymbol = new MethodSymbol(methodName, c.getSymbol).setPos(m)
        Enforce.methodDoesYetNotExitInClass(m, c)
        m.setSymbol(methodSymbol)
        m.id.setSymbol(methodSymbol)
        c.getSymbol.methods += methodName -> methodSymbol

        // Arguments
        m.args.foreach(a => {
          val variableName = a.id.value
          val variableSymbol = new VariableSymbol(variableName).setPos(a)
          a.setSymbol(variableSymbol)
          a.id.setSymbol(variableSymbol)
          m.getSymbol.argList = m.getSymbol.argList :+ variableSymbol
        })

        // Variables
        m.vars.foreach(v => {
          val variableName = v.id.value
          Enforce.varUniqueInScope(v, m)
          val variableSymbol = new VariableSymbol(variableName).setPos(v)
          v.setSymbol(variableSymbol)
          v.id.setSymbol(variableSymbol)
          m.getSymbol.members += variableName -> variableSymbol
        })
      })
    })
  }

  def symbolizeIdentifiers(p: Program): Unit = {
    symbolizeIdentifiers(p.main, p.main)
    p.classes.foreach(c => symbolizeIdentifiers(c, c))
  }

  def symbolizeIdentifiers(tree: Tree, scope : Symbolic[_ <: Symbol]) : Unit = {
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
      case v @ VarDecl(tpe, id, expr) =>
        symbolizeIdentifiers(tpe, scope)
        symbolizeIdentifiers(expr, scope)
        Enforce.assignConstantOrNew(v)
        Enforce.varUniqueInClassHierarchy(v, scope)
      case While(cond, body) => binaryTraverse(cond, body)
      case MainDecl(obj, parent, vars, exprs) =>
        vars.foreach(symbolizeIdentifiers(_, scope))
        exprs.foreach(symbolizeIdentifiers(_, scope))
      case ClassDecl(id, parent, vars, methods, _) =>
        vars.foreach(symbolizeIdentifiers(_, scope))
        methods.foreach(m => symbolizeIdentifiers(m, m))
      case m @ MethodDecl(overrides, retType, id, args, vars, exprs, retExpr, foreignPath) =>
        symbolizeIdentifiers(retType, scope)
        args.foreach(symbolizeIdentifiers(_, scope))
        Enforce.uniqueNames(args, scope)
        Enforce.ifOverridesExists(overrides, m)
        vars.foreach(symbolizeIdentifiers(_, scope))
        exprs.foreach(symbolizeIdentifiers(_, scope))
        symbolizeIdentifiers(retExpr, scope)
      case MethodCall(obj, meth, args) =>
        symbolizeIdentifiers(obj, scope)
        args.foreach(a => symbolizeIdentifiers(a, scope))
      case Assign(id, expr) =>
        symbolizeIdentifier(id, scope)
        symbolizeIdentifiers(expr, scope)
      case id @ Identifier(_) => symbolizeIdentifier(id, scope)
      case New(tpe, args) =>
        Enforce.classExists(tpe)
        symbolizeIdentifier(tpe, scope)
      case Not(expr) => symbolizeIdentifiers(expr, scope)
      case Formal(tpe, id) =>
        symbolizeIdentifiers(tpe, scope)
        symbolizeIdentifier(id, scope)
      case Block(exprs) => exprs.foreach(e => symbolizeIdentifiers(e, scope))
      case Println(expr) => symbolizeIdentifiers(expr, scope)
      case If(expr, thn, els) =>
        binaryTraverse(expr, thn)
        if(els.isDefined) symbolizeIdentifiers(els.get, scope)
      case t @ This() => t.setSymbol(scope.getSymbol match {
        case m : MethodSymbol => m.classSymbol
        case c : ClassSymbol => c
        case x => sys.error(s"'This' scope was $x")
      }) // Expressions only allowed in methods
      case IntType() | StringType() | BooleanType() | UnitType() | IntLit(_) | StringLit(_) | False() | True() | Null() => Unit
    }
  }

  def symbolizeIdentifier(id : Identifier, scope: Symbolic[_ <: Symbol]) : Unit = {
    scope.getSymbol.lookupVar(id.value) match {
      case Some(x) => id.setSymbol(x)
      case None => globalScope.lookupClass(id.value) match {
        case Some(x) => id.setSymbol(x)
        case None =>
          Reporter.error(s"'${id.value}' not defined", id)
      }
    }
  }

  def debug(): Any = {
    println("---------- SUMMARY ------------")
    println("MAIN CLASS:")
    println(globalScope.mainClass)
    println()
    println("GLOBAL CLASSES:")
    globalScope.classes.foreach(x => {
      println(x._2 + "(" + x._2.getType +") extends " + x._2.parent)
      x._2.members.foreach(y => {
        println("\t" + y._2+"("+y._2.getType+")")
      })
      println()
      x._2.methods.foreach(y => {
        println("\t" + y._2+"("+y._2.getType + ") overrides " + y._2.overridden + " (" + y._2.argList.mkString(", ") + ")")
        y._2.argList.foreach(z => {
          println(z + "("+z.getType+")")
        })
        y._2.members.foreach(z => {
          println("\t\t" + z._2 + "("+z._2.getType+")")
        })
      })
      println()
    })
    println("----------- END -------------")
    println()
  }
}
