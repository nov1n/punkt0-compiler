package punkt0
package code

import ast.Trees._
import analyzer.Symbols._
import analyzer.Types._
import cafebabe._
import AbstractByteCodes.{New, _}
import ByteCodes._
import cafebabe.ClassFileTypes.U2

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

  def generateVarDecl(v: VarDecl, ch: CodeHandler): CodeHandler = {
    ch
  }

  def generateExpr(e: ExprTree, ch: CodeHandler): CodeHandler = e match {
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

      generateExpr(en, ch) <<
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
      generateExpr(l, ch)
      generateExpr(r, ch)
      ch << IADD
    case Minus(l, r) =>
      generateExpr(l, ch)
      generateExpr(r, ch)
      ch << ISUB
    case Times(l, r) =>
      generateExpr(l, ch)
      generateExpr(r, ch)
      ch << IMUL
    case Div(l, r) =>
      generateExpr(l, ch)
      generateExpr(r, ch)
      ch << IDIV
//    case And(l, r) =>
//    case Or(l, r) =>
//    case LessThan(l, r) =>
//    case Equals(l, r) =>
//    case MethodCall(obj, meth, args) =>
//
//    case Identifier(value) =>
//    case This() =>
//    case Null() =>
//    case New(tpe: Identifier) =>
//    case Not(expr: ExprTree) =>
//
//    case Block(exprs) =>
//    case If(expr, thn, els) =>
//    case While(cond, body) =>
//    case Assign(id, expr) =>
  }

  private def generateMainClassFile(srcFile: String, main: MainDecl, dir: String): Unit = {
    val parentDir = if (dir.isEmpty) "./" else dir
    val classFile = new ClassFile(main.obj.value, None)
    classFile.setSourceFile(srcFile)

    classFile.addDefaultConstructor.codeHandler

    // Code handler for main
    val mainCH = classFile.addMainMethod.codeHandler

    // Vars
    main.vars.foreach(x => generateVarDecl(x, mainCH))

    // Exprs
    main.exprs.foreach(x => generateExpr(x, mainCH))

    // Return
    mainCH << RETURN

    // Generate

    mainCH.print
    mainCH.freeze
    classFile.writeToFile(s"$parentDir${classFile.className}.class")
  }
}
