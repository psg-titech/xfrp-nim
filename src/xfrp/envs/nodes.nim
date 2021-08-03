## Node (time-varying value) environments.

import tables, options
from sequtils import mapIt, unzip, concat, deduplicate, toSeq, filterIt
from strutils import join
import patty
import ".."/[syntax, types, codeinfos, errors, topsort, materials]
import operators

type
  XfrpNodeId = XfrpId

  XfrpNodeDescription = object
    id: WithCodeInfo[XfrpId]
    init: Option[WithCodeInfo[XfrpExpr]]
    case isInput: bool
    of true:
      inputType: WithCodeInfo[XfrpType]

    of false:
      innerTypeOpt: Option[WithCodeInfo[XfrpType]]
      update: WithCodeInfo[XfrpExpr]
      depsNow, depsAtLast: seq[XfrpNodeId]

  XfrpNodeDescriptionTable = TableRef[XfrpNodeId, XfrpNodeDescription]

  XfrpNodeEnv* = object
    tbl: XfrpNodeDescriptionTable
    sortedInnerNodeIds: seq[XfrpNodeId]
    inputNodeIds: seq[XfrpNodeId]

  XfrpNodeDependencyInfo = tuple
    depsNow, depsAtLast: seq[XfrpNodeId]


proc extractNodeDeps(nodeTbl: XfrpNodeDescriptionTable; exp: WithCodeInfo[XfrpExpr]): XfrpNodeDependencyInfo =
  match exp.val:
    ExprId(idAst):
      let id = idAst.val
      if id in nodeTbl:
        result.depsNow.add id

      # constants are also variables

    ExprAnnot(idAst, annotAst):
      let id = idAst.val
      match annotAst.val:
        AnnotAtLast:
          if id notin nodeTbl:
            let err = XfrpReferenceError.newException("At-last reference is only valid for nodes, but '" & id & "' is not a node.")
            err.causedBy(exp)
            raise err

          let node = nodeTbl[id]
          if node.init.isNone:
            let err = XfrpReferenceError.newException("The node '" & id & "' has no initial value althought its at-last value is referred.")
            err.causedBy(node.id, exp)
            raise err

          result.depsAtLast.add id

    ExprBin(_, termAsts):
      let (depsNow, depsAtLast) = termAsts.mapIt(nodeTbl.extractNodeDeps(it)).unzip()
      result.depsNow = deduplicate(concat(depsNow))
      result.depsAtLast = deduplicate(concat(depsAtLast))

    ExprIf(ifAstRef, thenAstRef, elseAstRef):
      let
        (ifDepsNow, ifDepsAtLast) = nodeTbl.extractNodeDeps(ifAstRef[])
        (thenDepsNow, thenDepsAtLast) = nodeTbl.extractNodeDeps(thenAstRef[])
        (elseDepsnow, elseDepsAtLast) = nodeTbl.extractNodeDeps(elseAstRef[])

      result.depsNow = deduplicate(ifDepsNow & thenDepsNow & elseDepsNow)
      result.depsAtLast = deduplicate(ifDepsAtLast & thenDepsAtLast & elseDepsAtLast)

    ExprApp(_, argAsts):
      let (depsNow, depsAtLast) = argAsts.mapIt(nodeTbl.extractNodeDeps(it)).unzip()

      result.depsNow = deduplicate(concat(depsNow))
      result.depsAtLast = deduplicate(concat(depsAtLast))

    ExprMagic(_, argAsts):
      let (depsNow, depsAtLast) = argAsts.mapIt(nodeTbl.extractNodeDeps(it)).unzip()

      result.depsNow = deduplicate(concat(depsNow))
      result.depsAtLast = deduplicate(concat(depsAtLast))

    _: return


proc getTopologicallySortedNodeList(nodeTbl: XfrpNodeDescriptionTable): seq[XfrpNodeId] =
  ## Return node ID list by topologically-sorted ordering.
  func referencesOf(n: XfrpNodeId): seq[XfrpNodeId] =
    nodeTbl[n].depsNow

  let graph: ReferenceGraph[XfrpNodeId] =
    (domain: toSeq(keys(nodeTbl)).filterIt(not nodeTbl[it].isInput), referencesOf: referencesOf)

  let referenceCycle = graph.getAnyReferenceCycle
  if referenceCycle.len > 0:
    let
      referenceCycleDiagram = (referenceCycle & referenceCycle[0]).join(" -> ")
      err = XfrpReferenceError.newException("A cycle reference is detected. At-last reference is useful for avoiding such a cycle. (" & referenceCycleDiagram & ")")

    for nodeId in referenceCycle:
      err.causedBy(nodeTbl[nodeId].id)

    raise err

  result = graph.topologicallySorted(toSeq(keys(nodeTbl)).filterIt(nodeTbl[it].isInput))


proc makeNodeEnvironment*(ast: XfrpModule; materialTbl: XfrpMaterials; opEnv: XfrpOpEnv): XfrpNodeEnv =
  let nodeTbl = newTable[XfrpNodeId, XfrpNodeDescription]()
  var inputNodeIds = newSeq[XfrpNodeId]()

  for inputNodeAst in ast.ins:
    let
      (idAst, tyAst, initAstOpt) = inputNodeAst.val.split()
      id = idAst.val

    if id in nodeTbl:
      let err = XfrpDefinitionError.newException("Redeclaration of an input node '" & id & "' is detected.")
      err.causedBy(idAst)
      raise err

    nodeTbl[id] = XfrpNodeDescription(id: idAst, init: initAstOpt, isInput: true, inputType: tyAst)
    inputNodeIds.add id

  for def in ast.defs:
    match def.val:
      DefNode(idAndTyOptAst, initAstOpt, bodyAst):
        let
          (idAst, typeAstOpt) = split(idAndTyOptAst.val)
          id = idAst.val

        if id in nodeTbl:
          let err = XfrpDefinitionError.newException("Node '" & id & "' is already defined.")
          err.causedBy(idAst)
          raise err

        nodeTbl[id] = XfrpNodeDescription(id: idAst, init: initAstOpt, isInput: false,
          innerTypeOpt: typeAstOpt, update: opEnv.reparseBinaryExpression(bodyAst, ast.moduleId.val, materialTbl))

      _:
        discard

  # Extract inner nodes' dependencies.
  for nodeDesc in mvalues(nodeTbl):
    if not nodeDesc.isInput:
      (nodeDesc.depsNow, nodeDesc.depsAtLast) = nodeTbl.extractNodeDeps(nodeDesc.update)

  let sortedInnerNodeIds = getTopologicallySortedNodeList(nodeTbl)

  result = XfrpNodeEnv(tbl: nodeTbl, sortedInnerNodeIds: sortedInnerNodeIds, inputNodeIds: inputNodeIds)


when isMainModule:
  import os, json, std/jsonutils
  from ".."/loaders import newXfrpLoader, load, loadMaterials
  import operators

  if paramCount() < 1:
    echo "Usage: nodes [filename]"
    quit QuitFailure

  try:
    let
      loader = newXfrpLoader(@[getCurrentDir()])
      ast = loader.load(paramStr(1), false)
      materials = loader.loadMaterials(ast)
      opEnv = makeOperatorEnvironmentFromModule(ast.val, materials)
      nodeEnv = makeNodeEnvironment(ast.val, materials, opEnv)

    echo pretty(nodeEnv.toJson())

  except XfrpLanguageError as err:
    stderr.writeLine "[", err.name, "] ", err.msg
    for info in err.causes:
      stderr.writeLine pretty(info)
