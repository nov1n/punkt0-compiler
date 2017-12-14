package punkt0

import java.io.File

case class Context(
  file: Option[File] = None,
  classPath: Option[String] = None,
  debug: Boolean = true,
  outDir: Option[File] = None,
  doEval: Boolean = false,
  doHelp: Boolean = false,
  doPrintMain: Boolean = false,
  doTokens: Boolean = false,
  doAST: Boolean = false,
  doASTree: Boolean = false,
  doSymbolIds: Boolean = false,
)
