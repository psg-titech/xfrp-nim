import os, parseopt
import xfrp/[loaders, envs, operators, typecheck, codeinfos, errors]
import xfrp/codegen/ccodegen

proc writeHelp =
  echo "usage: xfrp [<options>] <xfrp-module-file-path>"
  echo "options:"
  echo "  -h, --help           show help message"
  echo "  -t, --target=TARGET  change target to TARGET (default: c)"
  echo "      --nomain         prevent compiler from generating an entrypoint file"

when isMainModule:
  var
    showHelpFlag = false
    entryFileName: string
    target = "c"
    noMainFlag = false

  let params = commandLineParams()
  var optParser = initOptParser(params, {'h'}, @["help", "nomain"])

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

      of "nomain":
        noMainFlag = true

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
    let
      (entryDir, entryName, entryExt) = absolutePath(entryFileName).splitFile()
      loader = newXfrpLoader(@[entryDir])
      ast = loader.load(entryName & entryExt, false)
      opEnv = makeOperatorEnvironmentFromModule(ast.val)
      env = makeEnvironmentFromModule(ast.val).mapForExpr do (expAst: auto) -> auto:
        opEnv.reparseBinaryExpression(expAst)
      typeEnv = makeTypeEnvironmentFromEnvironment(env)

    case target
    of "c":
      let (frpFile, headerFile, mainFile) = ccodegen.codegen(env, typeEnv)

      writeFile(env.name.val & ".c", frpFile)
      writeFile(env.name.val & ".h", headerFile)
      if noMainFlag:
        discard
      elif fileExists("main.c"):
        writeFile("main.c.gen", mainFile)
      else:
        writeFile("main.c", mainFile)

    else:
      stderr.writeLine "Unknown target: ", target
      quit QuitFailure

  except XfrpLoadError as err:
    stderr.writeLine "[", err.name, "]", err.msg
    stderr.writeLine "Search paths:"
    for path in err.searchPaths:
      stderr.writeLine "  ", path

    for info in err.causes:
      stderr.writeLine pretty(info)

  except XfrpLanguageError as err:
    stderr.writeLine "[", err.name, "] ", err.msg

    for info in err.causes:
      stderr.writeLine pretty(info)

    quit QuitFailure
