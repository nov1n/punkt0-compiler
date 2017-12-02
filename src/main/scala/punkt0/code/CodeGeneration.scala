package punkt0
package code

import ast.Trees._
import analyzer.Types._
import cafebabe._
import AbstractByteCodes.{New => NewInst, _}
import ByteCodes._
import punkt0.analyzer.Symbols.MethodSymbol

object CodeGeneration extends Phase[Program, Unit] {

  def run(prog: Program)(ctx: Context): Unit = {

    /** Writes the proper .class file in a given directory. An empty string for dir is equivalent to "./". */
    def generateClassFile(sourceName: String, ct: ClassDecl, dir: String): Unit = {
      val parentDir = if (dir.isEmpty) "./" else dir
      val parentClass = if (ct.parent.isDefined) Some(ct.parent.get.value) else None

      val classFile = new cafebabe.ClassFile(ct.id.value, parentClass)
      classFile.setSourceFile(sourceName)
      println(s"CLASS ${ct.id.value}")

      classFile.addDefaultConstructor

      ct.vars.foreach(x => {
        classFile.addField(toJVMType(x.tpe.getType), x.id.value)
      })

      ct.methods.foreach(x => {
        val mh = classFile.addMethod(toJVMType(x.retType.getType), x.id.value, x.args.map(y => toJVMType(y.tpe.getType)))
        generateMethodCode(mh.codeHandler, x)
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

      val symbolsToVars = scala.collection.mutable.Map[String, Int]()

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
    def generateMethodCode(ch: CodeHandler, mt: MethodDecl): Unit = {
      val symbolsToVars = scala.collection.mutable.Map[String, Int]()

      // Allocate args to slots 1...n
      mt.args.foldLeft(1)((x, y) => {
        symbolsToVars += y.getSymbol.toString -> x
        x+1
      })

      // Vars
      mt.vars.foreach(x => generateVarDecl(x, ch, symbolsToVars))

      // Exprs
      mt.exprs.foreach(x => generateExpr(x, ch, symbolsToVars))

      // Retexpr
      generateExpr(mt.retExpr, ch, symbolsToVars)

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

  def generateVarDecl(v: VarDecl, ch: CodeHandler, symbolsToSlots : scala.collection.mutable.Map[String, Int]): CodeHandler = {
    generateExpr(v.expr, ch, symbolsToSlots)

    val var1 = ch.getFreshVar
    symbolsToSlots += v.getSymbol.toString -> var1
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

  def generateExpr(e: ExprTree, ch: CodeHandler, symbolsToSlots : scala.collection.mutable.Map[String, Int]): CodeHandler = e match {
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

      generateExpr(en, ch, symbolsToSlots) <<
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
          generateExpr(l, ch, symbolsToSlots)
          generateExpr(r, ch, symbolsToSlots)
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
          generateExpr(l, ch, symbolsToSlots)

          // Append left
          ch <<
            InvokeVirtual("java/lang/StringBuilder", "append", s"(${toJVMType(l.getType)})Ljava/lang/StringBuilder;") <<
            POP <<
            ALoad(var1)

          // Append right
          generateExpr(r, ch, symbolsToSlots)
          ch <<
            InvokeVirtual("java/lang/StringBuilder", "append", s"(${toJVMType(r.getType)})Ljava/lang/StringBuilder;") <<
            POP <<
            ALoad(var1) <<
            // Push the string on the stack
            InvokeVirtual("java/lang/StringBuilder", "toString", "()Ljava/lang/String;")
      }
    case Minus(l, r) =>
      generateExpr(l, ch, symbolsToSlots)
      generateExpr(r, ch, symbolsToSlots)
      ch << ISUB
    case Times(l, r) =>
      generateExpr(l, ch, symbolsToSlots)
      generateExpr(r, ch, symbolsToSlots)
      ch << IMUL
    case Div(l, r) =>
      generateExpr(l, ch, symbolsToSlots)
      generateExpr(r, ch, symbolsToSlots)
      ch << IDIV
    case Equals(l, r) =>
      generateExpr(l, ch, symbolsToSlots)
      generateExpr(r, ch, symbolsToSlots)

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
      generateExpr(l, ch, symbolsToSlots)
      generateExpr(r, ch, symbolsToSlots)

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
      generateExpr(expr, ch, symbolsToSlots)
      expr.getType match {
        case TClass(_) | TString => // TODO: fall back on putfield
          ch << AStore(symbolsToSlots(id.getSymbol.toString))
        case TInt | TBoolean =>
          ch << IStore(symbolsToSlots(id.getSymbol.toString))
        case x =>
          sys.error(s"Expr was $x. Should not happen.")
      }
    case Not(expr: ExprTree) =>
      generateExpr(expr, ch, symbolsToSlots)

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

      generateExpr(l, ch, symbolsToSlots)

      ch <<
        IfEq(falseLbl)

      generateExpr(r, ch, symbolsToSlots)

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

      generateExpr(l, ch, symbolsToSlots)

      ch <<
        IfEq(rightLbl) <<
        Ldc(1) <<
        Goto(trueLbl) <<
        Label(rightLbl)

      generateExpr(r, ch, symbolsToSlots)

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
      generateExpr(obj, ch, symbolsToSlots)

      args.foreach(x => generateExpr(x, ch, symbolsToSlots))
      ch <<
        InvokeVirtual(meth.getSymbol.asInstanceOf[MethodSymbol].classSymbol.name, meth.value, createMethodSignature(m))
    case id @ Identifier(_) =>
      id.getType match {
        case TClass(_) | TString =>
          val name = id.getSymbol.toString
          symbolsToSlots.get(name) match {
            case Some(x) => ch << ALoad(x)
            // Fallback on field if not found in method scope
            case None =>
              ch << GetField("Bar", name, toJVMType(id.getType))
              // TODO: Find a way to figure out the class here as first arg to getfield
          }
        case TInt | TBoolean =>
          val name = id.getSymbol.toString
          symbolsToSlots.get(name) match {
            case Some(x) => ch << ILoad(x)
            // Fallback on field if not found in method scope
            case None => ch <<
              GetField("Bar", name, toJVMType(id.getType))
          }
        case x =>
          sys.error(s"Expr was $x. Should not happen.")
      }
    case This() =>
      ch << ALOAD_0
    case Null() =>
      ch << ACONST_NULL
    case If(expr, thn, els) =>
      generateExpr(expr, ch, symbolsToSlots)

      val trueLbl = ch.getFreshLabel("true")
      val falseLbl = ch.getFreshLabel("false")

      ch <<
        IfEq(falseLbl)

      generateExpr(thn, ch, symbolsToSlots)

      ch <<
        Goto(trueLbl) <<
        Label(falseLbl)

      if(els.isDefined) generateExpr(els.get, ch, symbolsToSlots)

      ch <<
        Label(trueLbl)
    case Block(exprs) =>
      exprs.foreach(x => generateExpr(x, ch, symbolsToSlots))
      ch
    case While(cond, body) =>
      val endLbl = ch.getFreshLabel("end")
      val loopLbl = ch.getFreshLabel("loop")

      ch <<
        Label(loopLbl)

      generateExpr(cond, ch, symbolsToSlots)

      ch <<
        IfEq(endLbl)

      generateExpr(body, ch, symbolsToSlots)

      ch <<
        Goto(loopLbl) <<
        Label(endLbl)
  }

  def toJVMType(t: Type) : String = t match {
    case TUnit => "V"
    case TInt => "I"
    case TBoolean => "Z"
    case TString => "Ljava/lang/String;"
    case TClass(x) => x.name
    case x => sys.error(s"Type $x shouldn't occur.")
  }
}
