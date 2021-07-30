## XFRP module loader.

import tables, os
import lexer, parser, syntax, errors, codeinfos

type
  XfrpLoader* = ref object
    loaded: TableRef[XfrpModuleId, WithCodeInfo[XfrpModule]]
    includeDirs: seq[string]

  XfrpMaterials* = TableRef[XfrpModuleId, WithCodeInfo[XfrpModule]]


proc newXfrpLoader*(includeDirs: seq[string]): XfrpLoader =
  new result
  result.loaded = newTable[XfrpModuleId, WithCodeInfo[XfrpModule]]()
  result.includeDirs = includeDirs


proc loadFromExistingFile(filePath: string): WithCodeInfo[XfrpModule] =
  var l = buildLexerFromFilename(filePath)
  return parse(l)


proc load*(loader: XfrpLoader; name: string; traverseIncludeDirs = true): WithCodeInfo[XfrpModule] =
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

  if traverseIncludeDirs:
    for includePath in loader.includeDirs:
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


proc loadMaterials*(loader: XfrpLoader; ast: WithCodeInfo[XfrpModule]; history: seq[XfrpModuleId] = @[]): XfrpMaterials =
  result = newTable[XfrpModuleId, WithCodeInfo[XfrpModule]]()

  for depModule in ast.val.uses:
    if depModule.val in history:
      let err = XfrpReferenceError.newException("Cyclic module dependency is detected.")
      err.causedBy(depModule)
      raise err

    try:
      let
        depAst = loader.load(depModule.val)
        depMaterials = loader.loadMaterials(depAst, history & ast.val.moduleId.val)

      result[depModule.val] = depAst
      for (depMaterialId, depMaterialAst) in pairs(depMaterials):
        result[depMaterialId] = depMaterialAst

    except XfrpLoadError as err:
      err.causedBy(depModule)
      raise err


when isMainModule:
  import os, json, std/jsonutils

  if paramCount() < 1:
    stderr.writeLine "Usage: loaders [filename]"
    quit QuitFailure

  try:
    let
      loader = newXfrpLoader(@[getCurrentDir()])
      ast = loader.load(paramStr(1), false)
      materials = loader.loadMaterials(ast)

    echo pretty((ast: ast, materials: materials).toJson())

  except XfrpLoadError as err:
    stderr.writeLine "[", err.name, "] ", err.msg

    stderr.writeLine "Search:"
    for searchPath in err.searchPaths:
      stderr.writeLine "  ", searchPath
    stderr.writeLine ""

    for info in err.causes:
      stderr.writeLine pretty(info)

  except XfrpLanguageError as err:
    stderr.writeLine "[", err.name, "] ", err.msg

    for info in err.causes:
      stderr.writeLine pretty(info)
