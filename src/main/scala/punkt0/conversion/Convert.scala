package punkt0.conversion

import punkt0.analyzer.Types._
import punkt0.ast.Trees._

object Convert {
  def typedToTypeTree(tpe : Type) : TypeTree = {
    tpe match {
      case TUnit => UnitType()
      case TInt => IntType()
      case TString => StringType()
      case TBoolean => BooleanType()
      case TClass(x) => Identifier(x.name)
    }
  }

  def typeTreeToTyped(tpe: TypeTree): List[Type] = {
    tpe match {
      case BooleanType() => List(TBoolean)
      case IntType() => List(TInt)
      case UnitType() => List(TUnit)
      case StringType() => List(TString, TUntyped)
      case t => List(t.getType, TUntyped)
    }
  }

  def typeToJVMType(t: Type) : String = t match {
    case TUnit => "V"
    case TInt => "I"
    case TBoolean => "Z"
    case TString => "Ljava/lang/String;"
    case TClass(x) => s"L${x.name};"
    case x => sys.error(s"Type $x shouldn't occur.")
  }

}
