## Function environments.

import tables
from sequtils import toSeq, allIt, mapIt, concat, deduplicate
from strutils import join
import patty
import ".."/[syntax, types, codeinfos, errors, topsort, materials]
import operators

type
  XfrpFuncId = tuple
    module: XfrpModuleId
    id: XfrpId

  XfrpFuncDescription = object
    id: WithCodeInfo[XfrpId]
    module: WithCodeInfo[XfrpModuleId]
    retType: WithCodeInfo[XfrpType]
    args: seq[WithCodeInfo[XfrpIdAndType]]
    body: WithCodeInfo[XfrpExpr]
    deps: seq[XfrpFuncId]

  XfrpFuncDescriptionTable = TableRef[XfrpFuncId, XfrpFuncDescription]

  XfrpFuncEnv* = object
    tbl: XfrpFuncDescriptionTable
    sortedIds: seq[XfrpFuncId]


func `$`(funcId: XfrpFuncId): string =
  funcId.module & "." & funcId.id


proc extractFuncDeps(funcTbl: XfrpFuncDescriptionTable; exp: WithCodeInfo[XfrpExpr]; definedIn: XfrpModuleId; materialTbl: XfrpMaterials): seq[XfrpFuncId] =
  match exp.val:
    ExprApp(idAst, argAsts):
      let argDeps = argAsts.mapIt(funcTbl.extractFuncDeps(it, definedIn, materialTbl)).concat()
      for moduleId in materialTbl.materialsOf(definedIn):
        let funcId = (moduleId, idAst.val)

        if funcId in funcTbl:
          return deduplicate(funcId & argDeps)

      let err = XfrpReferenceError.newException("Function '" & idAst.val & "' is not defined.")
      err.causedBy(exp)
      raise err

    ExprBin(_, termAsts):
      let termDeps = termAsts.mapIt(funcTbl.extractFuncDeps(it, definedIn, materialTbl)).concat()
      result = deduplicate(termDeps)

    ExprIf(ifAstRef, thenAstRef, elseAstRef):
      let
        ifDeps = funcTbl.extractFuncDeps(ifAstRef[], definedIn, materialTbl)
        thenDeps = funcTbl.extractFuncDeps(thenAstRef[], definedIn, materialTbl)
        elseDeps = funcTbl.extractFuncDeps(elseAstRef[], definedIn, materialTbl)

      result = deduplicate(ifDeps & thenDeps & elseDeps)

    ExprMagic(_, argAsts):
      let argDeps = argAsts.mapIt(funcTbl.extractFuncDeps(it, definedIn, materialTbl)).concat()
      result = deduplicate(argDeps)

    ExprId(_):
      # constant variables may be also dependencies
      return

    _:
      return


proc getTopologicallySortedFuncList(funcTbl: XfrpFuncDescriptionTable): seq[XfrpFuncId] =
  ## Return function ID list by topologically-sorted ordering.
  func referencesOf(n: XfrpFuncId): seq[XfrpFuncId] =
    funcTbl[n].deps

  let graph: ReferenceGraph[XfrpFuncId] =
    (domain: toSeq(keys(funcTbl)), referencesOf: referencesOf)

  let referenceCycle = graph.getAnyReferenceCycle()
  if referenceCycle.len > 0:
    let
      referenceCycleDiagram = (referenceCycle & referenceCycle[0]).join(" -> ")
      err = XfrpReferenceError.newException("Any recursive functions are prohibited. (" & referenceCycleDiagram & ")")

    for funcId in referenceCycle:
      err.causedBy(funcTbl[funcId].id)

    raise err

  result = graph.topologicallySorted()

  assert(toSeq(keys(funcTbl)).allIt(it in result))


proc makeFunctionEnvironment*(ast: XfrpModule; materialTbl: XfrpMaterials; opEnv: XfrpOpEnv): XfrpFuncEnv =
  let functionTbl = newTable[XfrpFuncId, XfrpFuncDescription]()

  for moduleId in materialTbl.materialsOf(ast.moduleId.val):
    let module = materialTbl[moduleId]

    for def in module.val.defs:
      match def.val:
        DefFunc(idAst, retTyAst, argAsts, bodyAst):
          if (moduleId, idAst.val) in functionTbl:
            let err = XfrpDefinitionError.newException("Function '" & idAst.val & "' is already defined.")
            err.causedBy(def)
            raise err

          functionTbl[(moduleId, idAst.val)] =
            XfrpFuncDescription(id: idAst, module: module.val.moduleId, retType: retTyAst, args: argAsts, body: opEnv.reparseBinaryExpression(bodyAst, moduleId, materialTbl))

        _:
          discard

  for funcDesc in mvalues(functionTbl):
    funcDesc.deps = functionTbl.extractFuncDeps(funcDesc.body, funcDesc.module.val, materialTbl)

  let sortedFuncIds = getTopologicallySortedFuncList(functionTbl)

  result = XfrpFuncEnv(tbl: functionTbl, sortedIds: sortedFuncIds)


when isMainModule:
  import os, json, std/jsonutils
  from ".."/loaders import newXfrpLoader, load, loadMaterials
  import operators

  if paramCount() < 1:
    echo "Usage: functions [filename]"
    quit QuitFailure

  try:
    let
      loader = newXfrpLoader(@[getCurrentDir()])
      ast = loader.load(paramStr(1), false)
      materials = loader.loadMaterials(ast)
      opEnv = makeOperatorEnvironmentFromModule(ast.val, materials)
      funcEnv = makeFunctionEnvironment(ast.val, materials, opEnv)

    echo pretty(funcEnv.toJson())

  except XfrpLanguageError as err:
    stderr.writeLine "[", err.name, "] ", err.msg
    for info in err.causes:
      stderr.writeLine pretty(info)
