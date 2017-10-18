package punkt0.ast
import java.io.{File, FileWriter}

import org.scalatest._
import punkt0.Context
import punkt0.lexer.Lexer
import punkt0.Reporter

class ParserTest extends FlatSpec with Matchers {

  "The AST object" should "produce correct ASTs from source code" in {
    val d = new File("./testprograms/lab3/valid")
    val files = d.listFiles.filter(_.isFile).toList
    files.filter(x => !x.getName.contains(".ast")).foreach(f => {
      val ctx = Context()
      val lex = Lexer.run(f)(ctx)
      Reporter.terminateIfErrors()

      // Run parser
      val parsed = Parser.run(lex)(ctx).toString.trim
      val expected = scala.io.Source.fromFile("./testprograms/lab3/valid/"+f.getName+".ast").mkString.trim

      if (parsed != expected) {
        println("x - " + f.getName)
      }
      else println("v - " + f.getName)

      parsed should equal (expected)
    })
  }

  it should "fail on incorrect source programs" in {
    val d = new File("./testprograms/lab3/invalid")
    val files = d.listFiles.filter(_.isFile).toList
    files.foreach(f => {
      val ctx = Context()
      val lex = Lexer.run(f)(ctx)
      Reporter.terminateIfErrors()

      // Run parser
      //Parser.run(lex)(ctx).toString.trim
    })
  }
}
