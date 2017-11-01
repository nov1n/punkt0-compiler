package punkt0
package analyzer

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
  sealed abstract class Symbol extends Positioned {
    val id: Int = ID.next
    val name: String
  }

  // This object is an ID generator
  private object ID {
    private var c: Int = 0

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

    def lookupClass(n: String): Option[ClassSymbol] = ???
  }

  // This class represents the class scope with a parent, methods, and members
  class ClassSymbol(val name: String) extends Symbol {
    var parent: Option[ClassSymbol] = None
    var methods = Map[String, MethodSymbol]()
    var members = Map[String, VariableSymbol]()

    def lookupMethod(n: String): Option[MethodSymbol] = ???
    def lookupVar(n: String): Option[VariableSymbol] = ???
  }

  // This class represents a method scope with params and members
  class MethodSymbol(val name: String, val classSymbol: ClassSymbol) extends Symbol {
    var params = Map[String, VariableSymbol]()
    var members = Map[String, VariableSymbol]()
    var argList: List[VariableSymbol] = Nil
    var overridden: Option[MethodSymbol] = None

    def lookupVar(n: String): Option[VariableSymbol] = ???
  }

  // This class represents a variable scope (1)
  class VariableSymbol(val name: String) extends Symbol

}
