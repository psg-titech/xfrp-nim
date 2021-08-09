## XFRP compiler written in Nim.
##
## About XFRP
## ==========
##
## **XFRP** is a general-purpose functional reactive programming (FRP) language.
## The language specifications are based on `Emfrp <https://github.com/sawaken/emfrp>`_,
## a FRP language for small-scale embedded systems.
##
## Command-line options
## ====================
##
## -h, --help           show help message
## -t, --target=TARGET  change target to TARGET (default: c)
## -x, --extension=EXT  enable extension EXT
## --nomain             prevent compiler from generating an entrypoint file
##
## Build
## =====
##
## Nim compiler is required for build.
##
## .. code:: cmd
##
##   nimble build -d:release
##

import os, parseopt
import xfrp/[loaders, envs, codeinfos, errors, compilerflags]
import xfrp/codegen/ccodegen

proc writeHelp =
  echo "usage: xfrp [<options>] <xfrp-module-file-path>"
  echo "options:"
  echo "  -h, --help           show help message"
  echo "  -t, --target=TARGET  change target to TARGET (default: c)"
  echo "  -x, --extension=EXT  enable extension EXT"
  echo "      --nomain         prevent compiler from generating an entrypoint file"
  echo "available extensions:"
  echo "  -x=autoinit          initialization expression, automatic initialization"

when isMainModule:
  var
    showHelpFlag = false
    entryFileName: string
    target = "c"
    noMainFlag = false
    flags: set[CompilerFlag]

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

      of "x", "extension":
        case paramValue
        of "autoinit":
          flags.incl flagAutoInitExt

        else:
          stderr.writeLine "Unknown extension: ", paramValue
          quit QuitFailure

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
      (entryDir, _, _) = absolutePath(entryFileName).splitFile()
      loader = newXfrpLoader(@[entryDir, getAppDir() / "xfrp_include"])
      ast = loader.load(absolutePath(entryFileName), flags, false)
      materials = loader.loadMaterials(ast, flags)
      env = makeEnvironment(materials, flags)

    case target
    of "c":
      let (frpFile, headerFile, mainFile) = ccodegen.codegen(env)

      writeFile(env.name & ".c", frpFile)
      writeFile(env.name & ".h", headerFile)
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

    quit QuitFailure

  except XfrpLanguageError as err:
    stderr.writeLine "[", err.name, "] ", err.msg

    for info in err.causes:
      stderr.writeLine pretty(info)

    quit QuitFailure
