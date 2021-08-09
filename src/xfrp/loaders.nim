## XFRP module loader.
## A loader is actually the combination of a lexer and a parser with memo.
##
## **See also:**
## * `lexer <lexer.html>`_ for XFRP lexer
## * `parser <parser.html>`_ for XFRP parser
## * `materials <materials.html>`_ for XFRP materials


import tables, os
import lexer, parser, syntax, errors, codeinfos, materials, compilerflags

export materials

type
  XfrpLoader* = ref object
    ## File-to-AST loader.
    loaded: TableRef[XfrpModuleId, WithCodeInfo[XfrpModule]]
    includeDirs: seq[string]


proc newXfrpLoader*(includeDirs: seq[string]): XfrpLoader =
  ## Create new loader.
  new result
  result.loaded = newTable[XfrpModuleId, WithCodeInfo[XfrpModule]]()
  result.includeDirs = includeDirs


proc loadFromExistingFile(filePath: string; flags: set[CompilerFlag] = {}): WithCodeInfo[XfrpModule] =
  var l = buildLexerFromFilename(filePath)
  return parse(l, flags)


proc load*(loader: XfrpLoader; name: string; flags: set[CompilerFlag] = {}; traverseIncludeDirs = true): WithCodeInfo[XfrpModule] =
  ## Load a file and convert to an AST.
  ## When ``traverseIncludeDirs`` is true, the loader searches for the file in all possible paths.
  if name in loader.loaded:
    return loader.loaded[name]

  let (moduleDir, moduleName, moduleExt) = name.splitFile()

  if moduleName.len == 0:
    let err = XfrpLoadError.newException("File name is empty.")
    raise err

  let moduleFileName = if moduleExt.len == 0: moduleName & ".xfrp" else: moduleName & moduleExt

  var searchPaths: seq[string]

  if fileExists(moduleDir / moduleFileName):
    result = loadFromExistingFile(moduleDir / moduleFileName, flags)
    loader.loaded[name] = result
    return

  else:
    searchPaths.add moduleDir / moduleFileName

  if traverseIncludeDirs:
    for includePath in loader.includeDirs:
      let filePath = includePath / moduleDir / moduleFileName

      if fileExists(filePath):
        result = loadFromExistingFile(filePath, flags)
        loader.loaded[name] = result
        return

      else:
        searchPaths.add filePath

  let err = XfrpLoadError.newException("File not found.")
  err.searchPaths = searchPaths
  raise err


proc loadMaterials*(loader: XfrpLoader; ast: WithCodeInfo[XfrpModule]; flags: set[CompilerFlag] = {}; history: seq[XfrpModuleId] = @[]): XfrpMaterials =
  ## Load materials of given AST.
  var tbl = newTable[XfrpModuleId, WithCodeInfo[XfrpModule]]()
  tbl[ast.val.moduleId.val] = ast

  for depModule in ast.val.uses:
    if depModule.val in history:
      let err = XfrpReferenceError.newException("Cyclic module dependency is detected.")
      err.causedBy(depModule)
      raise err

    try:
      let
        depAst = loader.load(depModule.val, flags)
        depMaterials = loader.loadMaterials(depAst, flags, history & ast.val.moduleId.val)

      for (depMaterialId, depMaterialAst) in pairs(depMaterials):
        tbl[depMaterialId] = depMaterialAst

    except XfrpLoadError as err:
      err.causedBy(depModule)
      raise err

  result = makeMaterials(tbl, ast.val.moduleId.val)
