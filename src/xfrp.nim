import os, parseopt
import xfrp/[lexer, parser, envs, typecheck, codeinfos, errors]
import xfrp/codegen/ccodegen

proc writeHelp =
  echo "usage: xfrp [<options>] <xfrp-module-file-path>"
  echo "options:"
  echo "  -h, --help           show help message"
  echo "  -t, --target=TARGET  change target to TARGET (default: c)"

when isMainModule:
  var
    showHelpFlag = false
    entryFileName: string
    target = "c"

  let params = commandLineParams()
  var optParser = initOptParser(params, {'h'}, @["help"])

  for paramKind, paramKey, paramValue in optParser.getopt:
    case paramKind
    of cmdArgument:
      entryFileName = paramKey

    of cmdShortOption, cmdLongOption:
      case paramKey
      of "h", "help":
        showHelpFlag = true

      of "t", "target":
        target = paramValue

      else:
        stderr.writeLine "Unknown option: " , paramKey
        quit QuitFailure

    of cmdEnd: assert false

  if showHelpFlag:
    writeHelp()
    quit QuitSuccess

  if entryFileName.len == 0:
    writeHelp()
    quit QuitFailure

  try:
    var l = buildLexerFromFilename(entryFileName)
    let
      ast = parse(l)
      env = makeEnvironmentFromModule(ast.val)
      typeEnv = makeTypeEnvironmentFromEnvironment(env)

    case target
    of "c":
      let (frpFile, headerFile, mainFile) = ccodegen.codegen(env, typeEnv)

      writeFile("Example.c", frpFile)
      writeFile("Example.h", headerFile)
      writeFile("main.c", mainFile)

    else:
      stderr.writeLine "Unknown target: ", target
      quit QuitFailure

  except XfrpLanguageError as err:
    stderr.writeLine "[", err.name, "] ", err.msg

    for info in err.causes:
      stderr.writeLine pretty(info)

    quit QuitFailure
