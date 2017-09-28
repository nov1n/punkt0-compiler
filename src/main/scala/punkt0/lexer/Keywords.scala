package punkt0.lexer

object Keywords {

  val keywords : Map[String, TokenKind] = Map(
    "object" -> OBJECT,
    "class" -> CLASS,
    "def" -> DEF,
    "override" -> OVERRIDE,
    "var" -> VAR,
    "Unit" -> UNIT,
    "String" -> STRING,
    "extends" -> EXTENDS,
    "Int" -> INT,
    "Boolean" -> BOOLEAN,
    "while" -> WHILE,
    "if" -> IF,
    "else" -> ELSE,
    "true" -> TRUE,
    "false" -> FALSE,
    "this" -> THIS,
    "null" -> NULL,
    "new" -> NEW,
    "println" -> PRINTLN,
  )

  def lookup(s : String) : Option[TokenKind] = keywords.get(s)
}
