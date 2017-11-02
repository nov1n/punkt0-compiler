package punkt0
package analyzer

import ast.Trees._
import Symbols._

object NameAnalysis extends Phase[Program, Program] {

  def run(prog: Program)(ctx: Context): Program = {
    import Reporter._

    // Step 1: Collect symbols in declarations

    // Main
    prog.main.parent.setSymbol(new ClassSymbol(prog.main.parent.value))
    prog.main.obj.setSymbol(new ClassSymbol(prog.main.obj.value))
    prog.main.vars.foreach(v => v.setSymbol(new VariableSymbol(v.id.value)))

    // Classes
    prog.classes.foreach(c => {
      val className = c.id.value
      val classSymbol = new ClassSymbol(className)

      // Classes
      c.setSymbol(classSymbol)

      // Variables
      c.vars.foreach(v => v.setSymbol(new VariableSymbol(v.id.value)))

      c.methods.foreach(m => {
        // Arguments
        m.args.foreach(a => a.setSymbol(new VariableSymbol(a.id.value)))

        // Methods
        val ms = new MethodSymbol(m.id.value, classSymbol)
        m.setSymbol(ms)

        // Variables
        m.vars.foreach(v => v.setSymbol(new VariableSymbol(v.id.value)))
      })
    })

    // Step 2: Attach symbols to identifiers (except method calls) in method bodies

    // (Step 3:) Print tree with symbol ids for debugging

    // Make sure you check all constraints

    prog
  }

}
