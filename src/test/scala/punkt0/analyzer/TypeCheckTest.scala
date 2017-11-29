package punkt0.analyzer
import org.scalatest._
import Symbols._
import punkt0.analyzer.Types._

class TypeCheckTest extends FlatSpec with Matchers{
  "Calculate lowest upper bound method" should "return the correct result" in {
    val a: ClassSymbol = new ClassSymbol("A")
    a.setType(TClass(a))
    val b: ClassSymbol = new ClassSymbol("B")
    b.setType(TClass(b))
    val c: ClassSymbol = new ClassSymbol("C")
    c.setType(TClass(c))
    val d: ClassSymbol = new ClassSymbol("D")
    d.setType(TClass(d))
    val e: ClassSymbol = new ClassSymbol("E")
    e.setType(TClass(e))
    val f: ClassSymbol = new ClassSymbol("F")
    f.setType(TClass(f))

    //   AnyRef
    //     |   \
    //     E    \
    //    / \    |
    //   A   C   F
    //  / \
    // B   D

    b.parent = Some(a)
    d.parent = Some(a)
    a.parent = Some(e)
    c.parent = Some(e)

    TypeChecking.calcLeastUpperBound(e.getType, c.getType) should equal (e.getType)
    TypeChecking.calcLeastUpperBound(c.getType, e.getType) should equal (e.getType)

    TypeChecking.calcLeastUpperBound(b.getType, c.getType) should equal (e.getType)
    TypeChecking.calcLeastUpperBound(c.getType, b.getType) should equal (e.getType)

    TypeChecking.calcLeastUpperBound(b.getType, d.getType) should equal (a.getType)
    TypeChecking.calcLeastUpperBound(d.getType, b.getType) should equal (a.getType)

    TypeChecking.calcLeastUpperBound(d.getType, c.getType) should equal (e.getType)
    TypeChecking.calcLeastUpperBound(c.getType, d.getType) should equal (e.getType)

    TypeChecking.calcLeastUpperBound(a.getType, c.getType) should equal (e.getType)
    TypeChecking.calcLeastUpperBound(c.getType, a.getType) should equal (e.getType)

    TypeChecking.calcLeastUpperBound(f.getType, c.getType) should equal (anyRef)
    TypeChecking.calcLeastUpperBound(a.getType, f.getType) should equal (anyRef)
    TypeChecking.calcLeastUpperBound(f.getType, d.getType) should equal (anyRef)
    TypeChecking.calcLeastUpperBound(d.getType, f.getType) should equal (anyRef)

  }

  it should "return AnyRef for all primitive types" in {
    val clas: ClassSymbol = new ClassSymbol("class")
    clas.setType(TClass(clas))
    val str: VariableSymbol = new VariableSymbol("str")
    str.setType(TString)
    val int: VariableSymbol = new VariableSymbol("int")
    int.setType(TInt)
    val bool: VariableSymbol = new VariableSymbol("bool")
    bool.setType(TBoolean)
    val unit: VariableSymbol = new VariableSymbol("unit")
    unit.setType(TUnit)

    TypeChecking.calcLeastUpperBound(str.getType, clas.getType) should equal (anyRef)
    TypeChecking.calcLeastUpperBound(int.getType, clas.getType) should equal (anyRef)
    TypeChecking.calcLeastUpperBound(bool.getType, clas.getType) should equal (anyRef)
    TypeChecking.calcLeastUpperBound(unit.getType, clas.getType) should equal (TError)
    TypeChecking.calcLeastUpperBound(clas.getType, bool.getType) should equal (anyRef)
    TypeChecking.calcLeastUpperBound(clas.getType, unit.getType) should equal (TError)
    TypeChecking.calcLeastUpperBound(str.getType, int.getType) should equal (anyRef)
    TypeChecking.calcLeastUpperBound(bool.getType, unit.getType) should equal (TError)

  }
}
