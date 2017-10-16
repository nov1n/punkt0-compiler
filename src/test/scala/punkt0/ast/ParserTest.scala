package punkt0.ast
import java.io.File

import org.scalatest._
import punkt0.Context
import punkt0.lexer.Lexer
import punkt0.Reporter
import scala.io._

class ParserTest extends FlatSpec with Matchers {

  "The AST object" should "produce correct ASTs from source code" in {
    val d = new File("./testprograms/lab3/valid")
    val files = d.listFiles.filter(_.isFile).toList
    files.foreach(f => {
      val ctx = Context()
      val lex = Lexer.run(f)(ctx)
      //Reporter.terminateIfErrors()

      // Run parser
      Parser.run(lex)(ctx).toString should equal (scala.io.Source.fromFile("./testprograms/lab3/valid/"+f.getName).mkString)
    })
  }
}
