package punkt0
package code

import ast.Trees._
import analyzer.Types._
import cafebabe._
import AbstractByteCodes.{New => NewInst, _}
import ByteCodes._
import punkt0.analyzer.Symbols.MethodSymbol
import scala.collection.mutable.{Map => MuMap}

object CodeGeneration extends Phase[Program, Unit] {

  def run(prog: Program)(ctx: Context): Unit = {

    /** Writes the proper .class file in a given directory. An empty string for dir is equivalent to "./". */
    def generateClassFile(sourceName: String, ct: ClassDecl, dir: String): Unit = {
      val parentDir = if (dir.isEmpty) "./" else dir
      val parentClass = ct.getSymbol.parent match {
        case Some(x) => x.name
        case None => "java/lang/Object"
      }

      val classFile = new cafebabe.ClassFile(ct.id.value, Some(parentClass))
      classFile.setSourceFile(sourceName)

      val constructor = classFile.addConstructor(Nil).codeHandler

      // Create constructor
      val symbolsToRefs = MuMap[String, SymbolReference]()
      constructor <<
      ALOAD_0 <<
      InvokeSpecial(parentClass, "<init>", "()V") // Call parent constructor

      // Initialize fields
      ct.vars.foreach(x => {
        val fieldName = x.getSymbol.toString
        val className = ct.id.value
        classFile.addField(toJVMType(x.tpe.getType), fieldName)
        constructor << ALOAD_0
        generateExpr(x.expr, constructor, MuMap[String, SymbolReference]())
        constructor << PutField(className, fieldName, toJVMType(x.getSymbol.getType))
        symbolsToRefs += fieldName -> Field(ct.id.value)
      })
      constructor << RETURN

      println(s"\nCLASS ${ct.id.value}")
      println(s"METHOD ${ct.id.value}()")
      constructor.freeze
      constructor.print

      ct.methods.foreach(x => {
        val mh = classFile.addMethod(toJVMType(x.retType.getType), x.id.value, x.args.map(y => toJVMType(y.tpe.getType)))
        generateMethodCode(mh.codeHandler, x, symbolsToRefs)
      })
      classFile.writeToFile(s"$parentDir${ct.id.value}.class")
    }

    def generateMainClassFile(srcFile: String, main: MainDecl, dir: String): Unit = {
      val parentDir = if (dir.isEmpty) "./" else dir
      val classFile = new ClassFile(main.obj.value, None)
      classFile.setSourceFile(srcFile)
      classFile.addDefaultConstructor.codeHandler

      // Code handler for main
      val mainCH = classFile.addMainMethod.codeHandler

      val symbolsToVars = MuMap[String, SymbolReference]()

      // Vars
      main.vars.foreach(x => generateVarDecl(x, mainCH, symbolsToVars))

      // Exprs
      main.exprs.foreach(x => generateExpr(x, mainCH, symbolsToVars))

      // Return
      mainCH << RETURN

      // Generate
      println(s"\nCLASS ${main.obj.value}")
      println(s"METHOD main(${mainCH.paramTypes})")
      mainCH.print
      mainCH.freeze
      classFile.writeToFile(s"$parentDir${classFile.className}.class")
    }

    // a mapping from variable symbols to positions in the local variables
    // of the stack frame
    def generateMethodCode(ch: CodeHandler, mt: MethodDecl, symbolsToRefs : MuMap[String, SymbolReference]): Unit = {
      // Allocate args to slots 1...n
      mt.args.foldLeft(1)((x, y) => {
        symbolsToRefs += y.getSymbol.toString -> Var(x)
        x+1
      })

      // Vars
      mt.vars.foreach(x => generateVarDecl(x, ch, symbolsToRefs))

      // Exprs
      mt.exprs.foreach(x => generateExpr(x, ch, symbolsToRefs))

      // Retexpr
      generateExpr(mt.retExpr, ch, symbolsToRefs)

      // Return
      val ret = mt.retType.getType match {
        case TInt | TBoolean => IRETURN
        case TClass(_) | TString => ARETURN
        case x =>
          sys.error(s"Expr was $x. Should not happen.")
      }
      ch << ret

      println(s"METHOD ${mt.id.value}(${ch.paramTypes})")
      ch.print
      ch.freeze
    }

    // Driver
    val outDir = ctx.outDir.map(_.getPath + "/").getOrElse("./")

    val f = new java.io.File(outDir)
    if (!f.exists()) {
      f.mkdir()
    }

    val sourceName = ctx.file.get.getName

    // One classfile per class
    prog.classes foreach {
      ct => generateClassFile(sourceName, ct, outDir)
    }

    // One classfile for main
    generateMainClassFile(sourceName, prog.main, outDir)
  }

  def generateVarDecl(v: VarDecl, ch: CodeHandler, symbolsToSlots : MuMap[String, SymbolReference]): CodeHandler = {
    generateExpr(v.expr, ch, symbolsToSlots)

    val var1 = ch.getFreshVar
    symbolsToSlots += v.getSymbol.toString -> Var(var1)
    v.tpe.getType match {
      case TClass(_) | TString =>
        ch << AStore(var1)
      case TInt | TBoolean =>
        ch << IStore(var1)
      case x =>
        sys.error(s"Expr was $x. Should not happen.")
    }
  }

  def createMethodSignature(m: MethodCall) : String = {
    s"(${m.args.map(x => toJVMType(x.getType)).mkString("")})${toJVMType(m.getType)}"
    // E.g. `void foo(int a, boolean b)` ==> (IZ)V
  }

  def generateExpr(e: ExprTree, ch: CodeHandler, symbolsToRefs : MuMap[String, SymbolReference]): CodeHandler = e match {
    case Println(en) =>
      val tpe = toJVMType(en.getType)

      val var1 = ch.getFreshVar
      val var2 = ch.getFreshVar

      ch <<
        NewInst("java/lang/StringBuilder") <<
        DUP <<
        InvokeSpecial("java/lang/StringBuilder", "<init>", "()V") <<
        AStore(var1) <<
        ALoad(var1)

      generateExpr(en, ch, symbolsToRefs) <<
        InvokeVirtual("java/lang/StringBuilder", "append", s"($tpe)Ljava/lang/StringBuilder;") <<
        POP <<
        ALoad(var1) <<
        InvokeVirtual("java/lang/StringBuilder", "toString", "()Ljava/lang/String;") <<
        AStore(var2) <<
        GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;") <<
        ALoad(var2) <<
        InvokeVirtual("java/io/PrintStream", "println", "(Ljava/lang/String;)V")
    case True() =>
      ch << Ldc(1)
    case False() =>
      ch << Ldc(0)
    case StringLit(v) =>
      ch << Ldc(v)
    case IntLit(v) =>
      ch << Ldc(v)
    case Plus(l, r) =>
      (l.getType, r.getType) match {
        case (TInt, TInt) =>
          generateExpr(l, ch, symbolsToRefs)
          generateExpr(r, ch, symbolsToRefs)
          ch << IADD
        case (_, _) =>
          val var1 = ch.getFreshVar
          // Create strinbuilder
          ch <<
            NewInst("java/lang/StringBuilder") <<
            DUP <<
            InvokeSpecial("java/lang/StringBuilder", "<init>", "()V") <<
            AStore(var1) <<
            ALoad(var1)
          generateExpr(l, ch, symbolsToRefs)

          // Append left
          ch <<
            InvokeVirtual("java/lang/StringBuilder", "append", s"(${toJVMType(l.getType)})Ljava/lang/StringBuilder;") <<
            POP <<
            ALoad(var1)

          // Append right
          generateExpr(r, ch, symbolsToRefs)
          ch <<
            InvokeVirtual("java/lang/StringBuilder", "append", s"(${toJVMType(r.getType)})Ljava/lang/StringBuilder;") <<
            POP <<
            ALoad(var1) <<
            // Push the string on the stack
            InvokeVirtual("java/lang/StringBuilder", "toString", "()Ljava/lang/String;")
      }
    case Minus(l, r) =>
      generateExpr(l, ch, symbolsToRefs)
      generateExpr(r, ch, symbolsToRefs)
      ch << ISUB
    case Times(l, r) =>
      generateExpr(l, ch, symbolsToRefs)
      generateExpr(r, ch, symbolsToRefs)
      ch << IMUL
    case Div(l, r) =>
      generateExpr(l, ch, symbolsToRefs)
      generateExpr(r, ch, symbolsToRefs)
      ch << IDIV
    case Equals(l, r) =>
      generateExpr(l, ch, symbolsToRefs)
      generateExpr(r, ch, symbolsToRefs)

      val trueLbl = ch.getFreshLabel("equal")
      val falseLbl = ch.getFreshLabel("notEqual")

      l.getType match {
        case TInt | TBoolean =>
          ch <<
            If_ICmpEq(trueLbl)
        case _ =>
          ch <<
            If_ACmpEq(trueLbl)
      }
      ch <<
        Ldc(0) <<
        Goto(falseLbl) <<
        Label(trueLbl) <<
        Ldc(1) <<
        Label(falseLbl)
    case LessThan(l, r) =>
      generateExpr(l, ch, symbolsToRefs)
      generateExpr(r, ch, symbolsToRefs)

      val trueLbl = ch.getFreshLabel("lessThan")
      val falseLbl = ch.getFreshLabel("greaterOrEqual")
      ch <<
        If_ICmpLt(trueLbl) <<
        Ldc(0) <<
        Goto(falseLbl) <<
        Label(trueLbl) <<
        Ldc(1) <<
        Label(falseLbl)
    case Assign(id, expr) =>
      val name = id.getSymbol.toString

      // (A,I)Store or PUTFIELD
      symbolsToRefs.get(name) match {
        case Some(x) => x match {
          case Var(slot) =>
            expr.getType match {
              case TClass(_) | TString =>
                generateExpr(expr, ch, symbolsToRefs)
                ch << AStore(slot)
              case TInt | TBoolean =>
                generateExpr(expr, ch, symbolsToRefs)
                ch << IStore(slot)
              case y =>
                sys.error(s"Expr was $y. Should not happen.")
            }
          case Field(clas) =>
            ch << ALOAD_0
            generateExpr(expr, ch, symbolsToRefs)
            ch << PutField(clas, name, toJVMType(id.getType))
        }
        case None => sys.error(s"Assigning to uninitialized variable ${id.value}.")
      }
    case Not(expr: ExprTree) =>
      generateExpr(expr, ch, symbolsToRefs)

      val trueLbl = ch.getFreshLabel("equal")
      val falseLbl = ch.getFreshLabel("notEqual")
      ch <<
        IfEq(trueLbl) <<
        Ldc(0) <<
        Goto(falseLbl) <<
        Label(trueLbl) <<
        Ldc(1) <<
        Label(falseLbl)
    case And(l, r) =>
      val trueLbl = ch.getFreshLabel("true")
      val falseLbl = ch.getFreshLabel("false")

      generateExpr(l, ch, symbolsToRefs)

      ch <<
        IfEq(falseLbl)

      generateExpr(r, ch, symbolsToRefs)

      ch <<
        IfEq(falseLbl) <<
        Ldc(1) <<
        Goto(trueLbl) <<
        Label(falseLbl) <<
        Ldc(0) <<
        Label(trueLbl)
    case Or(l, r) =>
      val rightLbl = ch.getFreshLabel("right")
      val trueLbl = ch.getFreshLabel("true")
      val falseLbl = ch.getFreshLabel("false")

      generateExpr(l, ch, symbolsToRefs)

      ch <<
        IfEq(rightLbl) <<
        Ldc(1) <<
        Goto(trueLbl) <<
        Label(rightLbl)

      generateExpr(r, ch, symbolsToRefs)

      ch <<
        IfEq(falseLbl) <<
        Ldc(1) <<
        Goto(trueLbl) <<
        Label(falseLbl) <<
        Ldc(0) <<
        Label(trueLbl)
    case New(tpe: Identifier) =>
      ch <<
        NewInst(tpe.value) <<
        DUP <<
        InvokeSpecial(tpe.value, "<init>", "()V")

    case m @ MethodCall(obj, meth, args) =>
      generateExpr(obj, ch, symbolsToRefs)

      args.foreach(x => generateExpr(x, ch, symbolsToRefs))
      ch <<
        InvokeVirtual(meth.getSymbol.asInstanceOf[MethodSymbol].classSymbol.name, meth.value, createMethodSignature(m))
    case id @ Identifier(_) =>
      id.getType match {
        case TClass(_) | TString =>
          val name = id.getSymbol.toString
          symbolsToRefs.get(name) match {
            case Some(x) => x match {
              case Var(slot) =>
                ch << ALoad(slot)
            case Field(clas) =>
              // Fallback on field if not found in method scope
              ch <<
                ALOAD_0 <<
                GetField(clas, name, toJVMType(id.getType))
            }
            case None => sys.error(s"No variable or field with name ${id.value} found.")
          }
        case TInt | TBoolean =>
          val name = id.getSymbol.toString
          symbolsToRefs.get(name) match {
            case Some(x) => x match {
              case Var(slot) =>
                ch << ILoad(slot)
              case Field(clas) => ch <<
                ALOAD_0 <<
                GetField(clas, name, toJVMType(id.getType))
            }
            case None => sys.error(s"No variable or field with name $name found.")
          }
        case x =>
          sys.error(s"Expr was $x. Should not happen.")
      }
    case This() =>
      ch << ALOAD_0
    case Null() =>
      ch << ACONST_NULL
    case If(expr, thn, els) =>
      generateExpr(expr, ch, symbolsToRefs)

      val trueLbl = ch.getFreshLabel("true")
      val falseLbl = ch.getFreshLabel("false")

      ch <<
        IfEq(falseLbl)

      generateExpr(thn, ch, symbolsToRefs)

      ch <<
        Goto(trueLbl) <<
        Label(falseLbl)

      if(els.isDefined) generateExpr(els.get, ch, symbolsToRefs)

      ch <<
        Label(trueLbl)
    case Block(exprs) =>
      exprs.foreach(x => generateExpr(x, ch, symbolsToRefs))
      ch
    case While(cond, body) =>
      val endLbl = ch.getFreshLabel("end")
      val loopLbl = ch.getFreshLabel("loop")

      ch <<
        Label(loopLbl)

      generateExpr(cond, ch, symbolsToRefs)

      ch <<
        IfEq(endLbl)

      generateExpr(body, ch, symbolsToRefs)

      ch <<
        Goto(loopLbl) <<
        Label(endLbl)
  }

  def toJVMType(t: Type) : String = t match {
    case TUnit => "V"
    case TInt => "I"
    case TBoolean => "Z"
    case TString => "Ljava/lang/String;"
    case TClass(x) => s"L${x.name};"
    case x => sys.error(s"Type $x shouldn't occur.")
  }
}

sealed trait SymbolReference
case class Var(slot : Int) extends SymbolReference
case class Field(clas: String) extends SymbolReference
