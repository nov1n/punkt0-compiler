package punkt0
package resolver

import java.io.{File, IOException}

import org.apache.bcel.classfile.{ClassParser, Method}
import org.apache.bcel.generic.{Type => BType}
import org.apache.bcel.util.ClassPath
import punkt0.ast.Trees._

object ForeignResolver extends Phase[Program, Program] {

  def bcelTypeToPunktTypeTree(t: BType) : TypeTree = {
    t match {
      case BType.BOOLEAN => BooleanType()
      case BType.VOID => UnitType()
      case BType.STRING => StringType()
      case BType.INT => IntType()
    }
  }

  def defaultExprForType(t: TypeTree): ExprTree = t match {
    case BooleanType() => False()
    case UnitType() => Println(StringLit(""))
    case StringType() => StringLit("")
    case IntType() => IntLit(0)
    case x => Reporter.error(s"Invalid type $x", t); Null()
  }

  def methodsAreEqual(method: MethodDecl, classFileMethod: Method): Boolean = {
    if(method.args.length != classFileMethod.getArgumentTypes.length) return false
    method.args.map(_.tpe)
      .zip(classFileMethod.getArgumentTypes)
      .::(method.retType, classFileMethod.getReturnType)
      .forall({case (x, y) => return x == bcelTypeToPunktTypeTree(y)})
  }

  override def run(p: Program)(ctx: Context): Program = {

    val cp = newClassPath(ctx.classPath)

    p.classes.foreach(c => {
      c.foreignPath match {
        case None => Unit
        case Some(x) =>
          try {
            // Try to parse the class file according to the import path
            val className = x
            val classFile = cp.getClassFile(className)
            val classParsed = new ClassParser(classFile.getInputStream, classFile.getPath).parse()

            // For all foreign methods we must find one in the classfile with the same signature
            c.methods.foreach(m => {
              classParsed.getMethods.filter(classFileMethod => {
                classFileMethod.getName.equals(m.id.value) &&
                  classFileMethod.isPublic &&
                  methodsAreEqual(m, classFileMethod)
              }) match {
                case matched if matched.isEmpty => Reporter.error(s"Method ${m.id.value} not found in foreign class ${c.id.value}", m)
                case _ => Unit
              }
            })

          } catch {
            case e: IOException => Reporter.error(s"Foreign class $x not found: $e")
          }
      }
    })
    p
  }

  def newClassPath(cpArg : Option[String]): ClassPath = cpArg match{
    case None => new ClassPath(System.getProperty("java.class.path"))
    case Some(x) => new ClassPath(System.getProperty("java.class.path") + File.pathSeparator + x);
  }
}

