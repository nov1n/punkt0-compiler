package punkt0.analyzer

import org.scalatest._
import Symbols._
import punkt0.analyzer.Types._

class TypesTest extends FlatSpec with Matchers {
  val classSymbol = new ClassSymbol("foo")
  val classType = TClass(classSymbol)

  val cs2: ClassSymbol = new ClassSymbol("parent2")
  cs2.setType(TClass(cs2))
  val cs1: ClassSymbol = new ClassSymbol("parent1")
  cs1.setType(TClass(cs1))
  val cso = new ClassSymbol("other")
  cso.setType(TClass(cso))
  val cto = TClass(cso)

  classSymbol.parent = Some(cs1)
  cs1.parent = Some(cs2)

  val types = Array(TBoolean, TInt, TUnit, TString, classType)

  "Types" should "be subtypes of themselves" in {
    for((_, i) <- types.zipWithIndex) {
      types(i).isSubTypeOf(types(i)) should equal (true)
    }
  }

  it should "not be subtypes of another type" in {
    for((_, i) <- types.zipWithIndex) {
      types(i).isSubTypeOf(types((i+1)%types.length)) should equal (false)
    }
  }

  "Classes" should "inherit from AnyRef" in {
    types(4).isSubTypeOf(anyRef) should equal (true)
  }

  it should "be a subtype of its parent" in {
    types(4).isSubTypeOf(types(4).asInstanceOf[TClass].classSymbol.parent.get.getType) should equal (true)
  }

  it should "be a subtype of its parents parent" in {
    types(4).isSubTypeOf(types(4).asInstanceOf[TClass].classSymbol.parent.get.getType.asInstanceOf[TClass].classSymbol.parent.get.getType) should equal (true)
  }

  it should "not be a subtype of another class" in {
    types(4).isSubTypeOf(cso.getType) should equal (false)
    cso.getType.isSubTypeOf(types(4)) should equal (false)
  }

  //TODO: TEst anyref
}
