package punkt0.analyzer

import punkt0.{Positioned, Reporter}
import punkt0.analyzer.Symbols.{ClassSymbol, MethodSymbol, Symbol, Symbolic, VariableSymbol}
import punkt0.analyzer.Types._
import punkt0.ast.Trees._

object Enforce {
  def ifOverridesExists(overrides: Boolean, m: MethodDecl) : Unit = {
    if(!overrides) return
    m.getSymbol.classSymbol.parent match {
      case Some(x) => x.lookupMethod(m.id.value) match {
        case Some(y) => Unit
        case None => Reporter.error(s"Overriding nonexisting method ${m.id.value}", m)
      }
      case None => Unit
    }
  }

  def notUnit(v: Typed with Positioned) : Unit = v.getType match {
    case TUnit =>
      Reporter.error(s"Variable $v cannot have type Unit")
    case _ => Unit
  }

  def varUniqueInClassHierarchy(v: VarDecl, scope: Symbolic[_]): Unit = {
    val scopeSymbol = scope.getSymbol
    val parent = scopeSymbol match {
      case s: ClassSymbol => s.parent
      case s: MethodSymbol => s.classSymbol.parent
      case _: VariableSymbol => sys.error(s"Variable symbol cannot be a scope ${v.posString}")
    }

    if(parent.isDefined) {
      val parentVarDecl = parent.get.lookupVar(v.id.value)
      if(parentVarDecl.isDefined) {
        // This variable was already defined in a class higher up the hierarchy
        Reporter.error(s"'${v.id.value}' is defined by a parent class of ${scope.getSymbol.asInstanceOf[Symbol].name}", v)
      }
    }
  }

  def methodDoesYetNotExitInClass(m: MethodDecl, c: ClassDecl): Unit = {
    val duplicateSym = c.getSymbol.methods.get(m.id.value)
    if(duplicateSym.isDefined) {
      Reporter.error(s"'${m.id.value}' is defined more than once. First definition at ${duplicateSym.get.posString}.", m)
    }
  }

  def irreflexiveTransitiveClosure(classes: List[ClassDecl]) : Unit = {
    classes.foreach(c => {
      val search = inheritanceHierarchy(c.getSymbol)
      if(search.distinct.size != search.size) {
        Reporter.error(s"inheritance cycle for class ${c.id.value}: ${search.map(_.name).mkString(" => ")}", c)
      }
    })
  }

  def inheritanceHierarchy(c : ClassSymbol) : List[ClassSymbol] = {
    var search = List[ClassSymbol]()
    var parentOpt = c.parent
    while(parentOpt.isDefined) {
      parentOpt match {
        case Some(x) if search.contains(x) =>
          search = search :+ x
          return search
        case Some(y) =>
          search = search :+ y
          parentOpt = y.parent
        case None => Unit
      }
    }
    search
  }

  def classUnique(c: ClassDecl, classes: Map[String, Symbols.ClassSymbol]): Unit = {
    reservedClassNames.find(x => x.classSymbol.name == c.id.value) match {
      case Some(x) =>
        Reporter.error(s"'${x.classSymbol.name}' is a reserved class name.", c)
        return
      case None => Unit
    }
    classes.get(c.id.value) match {
      case Some(x) => Reporter.error(s"'${c.id.value}' is defined more than once. First definition at ${x.posString}", c)
      case None => Unit
    }
  }

  def assignConstantOrNew(v: VarDecl): Unit = v.expr match {
    case Block(_) | If(_, _, _) | While(_, _) |Println(_) | Assign(_, _) => Reporter.error(s"illigal variable delcaration, should be a constant or new'", v.id)
    case _ => Unit
  }

  def methodConstraints(m: MethodDecl) : Unit = {
    // Enforce unique name in scope
    val parent = m.getSymbol.classSymbol.parent
    if(parent.isDefined) {
      val parentMethodDeclOpt = parent.get.lookupMethod(m.id.value)
      if (parentMethodDeclOpt.isDefined) {
        val parentMethodDecl = parentMethodDeclOpt.get

        // Enforce override modifier if it already exists in scope
        if(m.overrides) {
          // Enforce size of overridden arguments it the same as the parent
          if (m.getSymbol.argList.size != parentMethodDecl.argList.size) {
            Reporter.error(s"'${m.id.value}' overrides a method in superclass ${parentMethodDecl.classSymbol.name} with different number of arguments.", m)
          }

          // Enforce all arguments of overridden method have same type as parent
          for((a1, a2) <- m.getSymbol.argList.zip(parentMethodDecl.argList)) {
            if (a1.getType != a2.getType) {
              Reporter.error(s"'${m.id.value}' overrides a method in superclass ${parentMethodDecl.classSymbol.name} with different types of arguments: ${a1.getType}, ${a2.getType}.", m)
            }
          }

          // Enforce return types to match
          val r1 = m.getSymbol.getType
          val r2 =parentMethodDecl.getType
          if(r1 != r2) {
            Reporter.error(s"'${m.id.value}' overrides a method in superclass ${parentMethodDecl.classSymbol.name} with different types of return argument: $r1, $r2.", m)
          }
        } else {
          Reporter.error(s"'${m.id.value}' overrides a method in superclass ${parentMethodDecl.classSymbol.name} without override modifier.", m)
        }
      }
    }
  }

  def varUniqueInScope(v: VarDecl, scope : Symbolic[_]) : Unit = {
    // Allow method var to shadow class var
    var classSym : Option[VariableSymbol] = None
    scope match {
      case m @ MethodDecl(_, _, _, _, _, _, _) =>
        classSym = m.getSymbol.classSymbol.lookupVar(v.id.value)
      case _ => Unit
    }

    // Enforce unique name in global scope (class names)
    NameAnalysis.globalScope.lookupClass(v.id.value) match {
      case Some(x) => Reporter.error(s"'${v.id.value}' shadows a class defined at ${x.posString} which is not allowed.", v)
      case None => Unit
    }

    // Enforce unique name in scope
    val sym = scope.getSymbol.asInstanceOf[Symbol].lookupVar(v.id.value)
    sym match {
      case Some(x) if sym != classSym => Reporter.error(s"'${v.id.value}' is defined more than once. First definition at ${x.posString}.", v)
      case Some(_) => Unit // Method var shadows class var
      case None => Unit // Variable is unique in scope
    }
  }

  def uniqueNames(args: List[Formal], scope : Symbolic[_]): Unit = {
    val duplicates = args.groupBy(_.id.value).filter(_._2.size > 1).values
    duplicates.foreach(x => Reporter.error(s"'${x.head.id.value}' is defined more than once. First definition at ${x.head.posString}", x(1)))
  }

}
