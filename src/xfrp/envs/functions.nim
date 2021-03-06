## Function environments.

import tables
from sequtils import toSeq, allIt, mapIt, concat, deduplicate, anyIt
from strutils import join
import patty
import ".."/[syntax, types, codeinfos, errors, topsort, materials]
import operators

type
  XfrpFuncId* = tuple
    ## Function identifier.
    module: XfrpModuleId
    id: XfrpId

  XfrpFuncDescription* = object
    ## Details about a function.
    id: WithCodeInfo[XfrpId]
    module: WithCodeInfo[XfrpModuleId]
    retType: WithCodeInfo[XfrpType]
    args: seq[WithCodeInfo[XfrpIdAndType]]
    body: WithCodeInfo[XfrpExpr]
    deps: seq[XfrpFuncId]

  XfrpFuncDescriptionTable = TableRef[XfrpFuncId, XfrpFuncDescription]

  XfrpFuncEnv* = object
    ## A function environment.
    tbl: XfrpFuncDescriptionTable
    sortedIds: seq[XfrpFuncId]


func `$`*(funcId: XfrpFuncId): string =
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

    ExprUnary(_, termAstRef):
      let termDeps = funcTbl.extractFuncDeps(termAstRef[], definedIn, materialTbl)
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


proc makeFunctionEnvironment*(materialTbl: XfrpMaterials; opEnv: XfrpOpEnv): XfrpFuncEnv =
  ## Construct new function environment.
  let functionTbl = newTable[XfrpFuncId, XfrpFuncDescription]()

  for moduleId in materialTbl:
    let module = materialTbl[moduleId]

    for def in module.val.defs:
      match def.val:
        DefFunc(idAst, retTyAst, argAsts, bodyAst):
          if (moduleId, idAst.val) in functionTbl:
            let err = XfrpDefinitionError.newException("Function '" & idAst.val & "' is already defined.")
            err.causedBy(def)
            raise err

          functionTbl[(moduleId, idAst.val)] =
            XfrpFuncDescription(id: idAst, module: module.val.moduleId, retType: retTyAst, args: argAsts,
              body: opEnv.reparseBinaryExpression(bodyAst, moduleId, materialTbl))

        _:
          discard

  for funcDesc in mvalues(functionTbl):
    funcDesc.deps = functionTbl.extractFuncDeps(funcDesc.body, funcDesc.module.val, materialTbl)

  let sortedFuncIds = getTopologicallySortedFuncList(functionTbl)

  result = XfrpFuncEnv(tbl: functionTbl, sortedIds: sortedFuncIds)


func checkFuncValidity*(env: XfrpFuncEnv; exp: WithCodeInfo[XfrpExpr]; definedIn: XfrpModuleId; materialTbl: XfrpMaterials) =
  ## Check if functions are all valid.
  ## If validity check is passed, then return `true`.
  match exp.val:
    ExprApp(idAst, argAsts):
      let id = idAst.val

      for arg in argAsts:
        env.checkFuncValidity(arg, definedIn, materialTbl)

      for moduleId in materialTbl.materialsOf(definedIn):
        if (moduleId, id) in env.tbl: return

      let err = XfrpReferenceError.newException("Function '" & id & "' is not defined.")
      err.causedBy(idAst)
      raise err

    ExprBin(_, termAsts):
      assert(termAsts.len == 2)

      env.checkFuncValidity(termAsts[0], definedIn, materialTbl)
      env.checkFuncValidity(termAsts[1], definedIn, materialTbl)

    ExprUnary(_, termAstRef):
      env.checkFuncValidity(termAstRef[], definedIn, materialTbl)

    ExprIf(ifAstRef, thenAstRef, elseAstRef):
      env.checkFuncValidity(ifAstRef[], definedIn, materialTbl)
      env.checkFuncValidity(thenAstRef[], definedIn, materialTbl)
      env.checkFuncValidity(elseAstRef[], definedIn, materialTbl)

    ExprMagic(_, argAsts):
      for arg in argAsts:
        env.checkFuncValidity(arg, definedIn, materialTbl)

    _: return


proc getFunction*(env: XfrpFuncEnv; id: XfrpFuncId): XfrpFuncDescription =
  ## Get a function description by function ID.
  result = env.tbl[id]


proc findFuncId*(env: XfrpFuncEnv; id: XfrpId; definedIn: XfrpModuleId; materialTbl: XfrpMaterials): XfrpFuncId =
  ## Search available function ID in given module.
  for moduleId in materialTbl.materialsOf(definedIn):
    if (moduleId, id) in env.tbl:
      return (moduleId, id)

  raise XfrpLanguageError.newException("Function '" & id & "' is not defined in module '" & definedIn & "'.")


iterator items*(env: XfrpFuncEnv): XfrpFuncId =
  ## Iterate all functions by ID.
  for id in env.sortedIds:
    yield id

# getters

func id*(desc: XfrpFuncDescription): WithCodeInfo[XfrpId] = desc.id
func module*(desc: XfrpFuncDescription): WithCodeInfo[XfrpModuleId] = desc.module
func retType*(desc: XfrpFuncDescription): WithCodeInfo[XfrpType] = desc.retType
func args*(desc: XfrpFuncDescription): seq[WithCodeInfo[XfrpIdAndType]] = desc.args
func body*(desc: XfrpFuncDescription): WithCodeInfo[XfrpExpr] = desc.body
