## XFRP module loader.

import tables, os
import lexer, parser, syntax, errors

type
  XfrpLoader* = ref object
    loaded: TableRef[XfrpModuleId, XfrpAst[XfrpModule]]
    includePaths: seq[string]


proc newXfrpLoader*(includePaths: seq[string]): XfrpLoader =
  new result
  result.loaded = newTable[XfrpModuleId, XfrpAst[XfrpModule]]()
  result.includePaths = includePaths


proc loadFromExistingFile(filePath: string): XfrpAst[XfrpModule] =
  var l = buildLexerFromFilename(filePath)
  return parse(l)


proc load*(loader: XfrpLoader; name: string; traversal = true): XfrpAst[XfrpModule] =
  if name in loader.loaded:
    return loader.loaded[name]

  let (moduleDir, moduleName, moduleExt) = name.splitFile()

  if moduleName.len == 0:
    let err = XfrpLoadError.newException("File name is empty.")
    raise err

  let moduleFileName = if moduleExt.len == 0: moduleName & ".xfrp" else: moduleName & moduleExt

  var searchPaths: seq[string]

  if fileExists(moduleDir / moduleFileName):
    result = loadFromExistingFile(moduleDir / moduleFileName)
    loader.loaded[name] = result
    return

  else:
    searchPaths.add moduleDir / moduleFileName

  if traversal:
    for includePath in loader.includePaths:
      let filePath = includePath / moduleDir / moduleFileName

      if fileExists(filePath):
        result = loadFromExistingFile(filePath)
        loader.loaded[name] = result
        return

      else:
        searchPaths.add filePath

  let err = XfrpLoadError.newException("File not found.")
  err.searchPaths = searchPaths
  raise err
