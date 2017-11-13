package punkt0.analyzer

import punkt0.Reporter
import punkt0.analyzer.Symbols.{ClassSymbol, MethodSymbol, Symbol, Symbolic, VariableSymbol}
import punkt0.ast.Trees._

object Enforce {

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

  def classUnique(className: String, classes: Map[String, Symbols.ClassSymbol]): Unit = {
    classes.get(className) match {
      case Some(x) => Reporter.error(s"'$className' is defined more than once. First definition at ${x.posString}", x)
      case None => Unit
    }
  }

  def assignConstantOrNew(v: VarDecl): Unit = v.expr match {
    case Null() | New(_) | IntLit(_) | StringLit(_) | True() | False() | Null() => Unit
    case _ => Reporter.error(s"illigal variable delcaration, should be a constant or new'", v.id)
  }

  def methodUniqueInClassHierarchyOrOverrides(m: MethodDecl) : Unit = { // TODO: Test this
    // Enforce unique name in scope
    val parent = m.getSymbol.classSymbol.parent // Don't count in this class because it will always be defined
    if(parent.isDefined) {
      val parentMethodDecl = parent.get.lookupMethod(m.id.value)
      if (parentMethodDecl.isDefined) {
        if(m.overrides) {
          if (m.getSymbol.argList.size != parentMethodDecl.get.argList.size) {
            Reporter.error(s"'${m.id.value}' overrides a method in superclass ${parentMethodDecl.get.classSymbol.name} with different number of arguments.", m)
          }
        } else {
          Reporter.error(s"'${m.id.value}' overrides a method in superclass ${parentMethodDecl.get.classSymbol.name} without override modifier.", m)
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
