package punkt0
package code

import ast.Trees._
import analyzer.Types._
import cafebabe._
import AbstractByteCodes.{New, _}
import ByteCodes._

object CodeGeneration extends Phase[Program, Unit] {

  def run(prog: Program)(ctx: Context): Unit = {

    /** Writes the proper .class file in a given directory. An empty string for dir is equivalent to "./". */
    def generateClassFile(sourceName: String, ct: ClassDecl, dir: String): Unit = {
      // TODO: Create code handler, save to files ...
      val parentDir = if (dir.isEmpty) "./" else dir
      val parentClass = if (ct.parent.isDefined) Some(ct.parent.get.value) else None

      // CODEGEN
      //val codeHandler = classFile.addMethod()
      val classFile = new cafebabe.ClassFile(ct.id.value, parentClass)
      classFile.setSourceFile(sourceName)

      //      codeHandler <<
      //        GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;") <<
      //        Ldc("Hello world!") <<
      //        InvokeVirtual("java/io/PrintStream", "println", "(Ljava/lang/String;)V") <<
      //        RETURN
      //
      //      codeHandler.freeze
      //      classFile.writeToFile(parentDir + sourceName)

    }

    // a mapping from variable symbols to positions in the local variables
    // of the stack frame
    def generateMethodCode(ch: CodeHandler, mt: MethodDecl): Unit = {
      val methSym = mt.getSymbol

      // TODO: Emit code

      ch.freeze
    }

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

    generateMainClassFile(sourceName, prog.main, outDir)
  }

  def generateVarDecl(v: VarDecl, ch: CodeHandler, s2v : scala.collection.mutable.Map[String, Int]): CodeHandler = {
    generateExpr(v.expr, ch, s2v)

    val var1 = ch.getFreshVar
    s2v += v.getSymbol.toString -> var1
    v.tpe.getType match {
      case TClass(_) | TString =>
        ch << AStore(var1)
      case TInt | TBoolean =>
        ch << IStore(var1)
      case x =>
        sys.error(s"Expr was $x. Should not happen.")
    }
  }

  def generateExpr(e: ExprTree, ch: CodeHandler, s2v : scala.collection.mutable.Map[String, Int]): CodeHandler = e match {
    case Println(en) =>
      val tpe = en.getType match {
        case TInt => "I"
        case TBoolean => "Z"
        case TString => "Ljava/lang/String;"
      }

      val var1 = ch.getFreshVar
      val var2 = ch.getFreshVar

      ch <<
        New("java/lang/StringBuilder") <<
        DUP <<
        InvokeSpecial("java/lang/StringBuilder", "<init>", "()V") <<
        AStore(var1) <<
        ALoad(var1)

      generateExpr(en, ch, s2v) <<
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
    case Plus(l, r) => // TODO: Handle string case, switch on l type
      generateExpr(l, ch, s2v)
      generateExpr(r, ch, s2v)
      ch << IADD
    case Minus(l, r) =>
      generateExpr(l, ch, s2v)
      generateExpr(r, ch, s2v)
      ch << ISUB
    case Times(l, r) =>
      generateExpr(l, ch, s2v)
      generateExpr(r, ch, s2v)
      ch << IMUL
    case Div(l, r) =>
      generateExpr(l, ch, s2v)
      generateExpr(r, ch, s2v)
      ch << IDIV
    case Equals(l, r) => // TODO: Compare things other than ints
      generateExpr(l, ch, s2v)
      generateExpr(r, ch, s2v)

      val trueLbl = ch.getFreshLabel("equal")
      val falseLbl = ch.getFreshLabel("notEqual")
      ch <<
        If_ICmpEq(trueLbl) <<
        Ldc(0) <<
        Goto(falseLbl) <<
        Label(trueLbl) <<
        Ldc(1) <<
        Label(falseLbl)
    case LessThan(l, r) =>
      generateExpr(l, ch, s2v)
      generateExpr(r, ch, s2v)

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
      generateExpr(expr, ch, s2v)
      expr.getType match {
        case TClass(_) | TString =>
          ch << AStore(s2v(id.getSymbol.toString))
        case TInt | TBoolean =>
          ch << IStore(s2v(id.getSymbol.toString))
        case x =>
          sys.error(s"Expr was $x. Should not happen.")
      }
    case Not(expr: ExprTree) =>
      generateExpr(expr, ch, s2v)

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

      generateExpr(l, ch, s2v)

      ch <<
        IfEq(falseLbl)

      generateExpr(r, ch, s2v)

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

      generateExpr(l, ch, s2v)

      ch <<
        IfEq(rightLbl) <<
        Ldc(1) <<
        Goto(trueLbl) <<
        Label(rightLbl)

      generateExpr(r, ch, s2v)

      ch <<
        IfEq(falseLbl) <<
        Ldc(1) <<
        Goto(trueLbl) <<
        Label(falseLbl) <<
        Ldc(0) <<
        Label(trueLbl)
//    case MethodCall(obj, meth, args) =>
    case id @ Identifier(_) =>
      id.getType match {
        case TClass(_) | TString =>
          ch << ALoad(s2v(id.getSymbol.toString))
        case TInt | TBoolean =>
          ch << ILoad(s2v(id.getSymbol.toString))
        case x =>
          sys.error(s"Expr was $x. Should not happen.")
      }
    case This() =>
      ch << ALOAD_0 // TODO: TEST
    case Null() =>
      ch << ACONST_NULL // TODO: TEST
    case If(expr, thn, els) =>
      generateExpr(expr, ch, s2v)

      val trueLbl = ch.getFreshLabel("true")
      val falseLbl = ch.getFreshLabel("false")

      ch <<
        IfEq(falseLbl)

      generateExpr(thn, ch, s2v)

      ch <<
        Goto(trueLbl) <<
        Label(falseLbl)

      if(els.isDefined) generateExpr(els.get, ch, s2v)

      ch <<
        Label(trueLbl)
    case Block(exprs) =>
      exprs.foreach(x => generateExpr(x, ch, s2v))
      ch
//    case New(tpe: Identifier) =>
    case While(cond, body) =>
      val endLbl = ch.getFreshLabel("end")
      val loopLbl = ch.getFreshLabel("loop")

      ch <<
        Label(loopLbl)

      generateExpr(cond, ch, s2v)

      ch <<
        IfEq(endLbl)

      generateExpr(body, ch, s2v)

      ch <<
        Goto(loopLbl) <<
        Label(endLbl)
  }

  private def generateMainClassFile(srcFile: String, main: MainDecl, dir: String): Unit = {
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
    mainCH.print
    mainCH.freeze
    classFile.writeToFile(s"$parentDir${classFile.className}.class")
  }
}
