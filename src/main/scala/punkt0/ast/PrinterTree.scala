package punkt0.ast

import punkt0.ast.Trees.{Program, Tree}

// Adapted from https://github.com/Azard/Rattata/blob/master/src/main/scala/me/azard/rattata/Rattata.scala
object PrinterTree {
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

  def apply(input : Tree, names : Boolean) : String = {
    apply(input.toString, names)
  }

  def apply(input: String, names: Boolean): String = {
    if(names) print("Hurr"); sys.exit(1)
    var res = new StringBuilder
    var level = 0
    input.foreach {
      case '(' =>
        level += 1
        res.append("\n")
        if (showLine) {
          res.append(("|" + " "*(tabSize-1)) * (level-1))
          res.append("|" + "-"*(tabSize-1))
        } else {
          res.append(" " * tabSize * level)
        }
      case ')' =>
        level -= 1
      case ',' =>
        res.append("\n")
        if (showLine) {
          res.append(("|" + " "*(tabSize-1)) * (level-1))
          res.append("|" + "-"*(tabSize-1))
        } else {
          res.append(" " * tabSize * level)
        }
      case ' ' =>
      case f => res.append(f)
    }
    res.toString()
  }
}
