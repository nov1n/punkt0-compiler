package punkt0
package analyzer

import punkt0.analyzer.Types.Typed

object Symbols {

  // This is a trait that is mixed in with symbolic AST nodes
  trait Symbolic[S <: Symbol] {
    private var _sym: Option[S] = None

    def setSymbol(sym: S): this.type = {
      _sym = Some(sym)
      this
    }

    def getSymbol: S = _sym match {
      case Some(s) => s
      case None => sys.error("Accessing undefined symbol.")
    }
  }

  // This class represents a symbol with a name and an id
  sealed abstract class Symbol extends Positioned with Typed{
    val id: Int = ID.next
    val name: String
    def lookupVar(n : String): Option[VariableSymbol]
    override val toString : String = s"$name#$id"
  }

  // This object is an ID generator
  private object ID {
    private var c: Int = 1 // TODO: Changed this to 1 to match example output

    def next: Int = {
      val ret = c
      c = c + 1
      ret
    }
  }

  // This class represents the global scope containing all the classes
  class GlobalScope {
    var mainClass: ClassSymbol = _
    var classes = Map[String, ClassSymbol]()

    def lookupClass(n: String): Option[ClassSymbol] = classes.get(n)
  }

  // This class represents the class scope with a parent, methods, and members
  class ClassSymbol(val name: String) extends Symbol {
    var parent: Option[ClassSymbol] = None
    var methods = Map[String, MethodSymbol]()
    var members = Map[String, VariableSymbol]()

    def lookupMethod(n: String): Option[MethodSymbol] = {
      var hierarchy = List[Option[MethodSymbol]]()
      // Parent
      if(parent.isDefined) hierarchy = hierarchy :+ parent.get.lookupMethod(n)
      // Local
      hierarchy = hierarchy :+ methods.get(n)
      // Return closest binding
      hierarchy.reduce((res, cur) => if(cur.isDefined) cur else res)
    }

    def lookupVar(n: String): Option[VariableSymbol] = {
      var hierarchy = List[Option[VariableSymbol]]()
      // Parent
      if(parent.isDefined) hierarchy = hierarchy :+ parent.get.lookupVar(n)
      // Local
      hierarchy = hierarchy :+ members.get(n)
      // Return closest binding
      hierarchy.reduce((res, cur) => if(cur.isDefined) cur else res)
    }
  }

  // This class represents a method scope with params and members
  class MethodSymbol(val name: String, val classSymbol: ClassSymbol) extends Symbol {
    var params = Map[String, VariableSymbol]()
    var members = Map[String, VariableSymbol]()
    var argList: List[VariableSymbol] = Nil
    var overridden: Option[MethodSymbol] = None

    def lookupVar(n: String): Option[VariableSymbol] = {
      var hierarchy = List[Option[VariableSymbol]]()
      // Class
      hierarchy = hierarchy :+ classSymbol.lookupVar(n)
      // Arguments
      hierarchy = hierarchy :+ argList.find(a => a.name.equals(n))
      // Local
      hierarchy = hierarchy :+ members.get(n)
      // Return closest binding
      hierarchy.reduce((res, cur) => if(cur.isDefined) cur else res)
    }
  }

  // This class represents a variable scope (1)
  class VariableSymbol(val name: String) extends Symbol {
    // TODO: Verify if this is needed. We add lookupVar to the Symbol interface
    // TODO: such that we can call lookupVar on any symbol in the symbolizeIdentifiers method.
    def lookupVar(n: String): Option[VariableSymbol] = {
      n match {
        case _ if n == name => Some(this)
        case _ => None
      }
    }
  }

  // This is assigned when the symbol does not exist, so that we can still print it
  class SymbolNotExists extends Symbol {
    override val name: String = "DOES_NOT_EXIST"
    override def lookupVar(n: String): Option[VariableSymbol] = None
  }

}
