package punkt0.ast

import punkt0.ast.Trees.{Program, Tree}

// Adapted from https://github.com/Azard/Rattata/blob/master/src/main/scala/me/azard/rattata/Rattata.scala
object PrettyPrinter {
  private var showLine = true
  private var tabSize = 4

  def setShowLine(show: Boolean) = {
    showLine = show
    this
  }

  def setTabSize(size: Int) = {
    if (size > 0) {
      tabSize = size
    } else {
      println("Warning: setTabSize only receive positive number")
    }
    this
  }

  def pprintAST(input: Tree): Unit = {
    var level = 0
    input.toString.foreach {
      case '(' =>
        level += 1
        println()
        if (showLine) {
          print(("|" + " "*(tabSize-1)) * (level-1))
          print("|" + "-"*(tabSize-1))
        } else {
          print(" " * tabSize * level)
        }
      case ')' =>
        level -= 1
      case ',' =>
        println()
        if (showLine) {
          print(("|" + " "*(tabSize-1)) * (level-1))
          print("|" + "-"*(tabSize-1))
        } else {
          print(" " * tabSize * level)
        }
      case ' ' =>
      case f => print(f)
    }
  }

}
