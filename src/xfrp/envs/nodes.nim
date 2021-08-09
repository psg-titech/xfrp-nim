## Node (time-varying value) environments.

import tables, options
from sequtils import mapIt, unzip, concat, deduplicate, toSeq, filterIt
from strutils import join
from algorithm import reversed
import patty
import ".."/[syntax, types, codeinfos, errors, topsort, materials, compilerflags]
import operators

type
  XfrpNodeId* = XfrpId ## Node identifier.

  XfrpNodeDescription* = object
    ## Details about a node.
    ## It can describe both input and inner node.
    id: WithCodeInfo[XfrpId]
    initOpt: Option[WithCodeInfo[XfrpExpr]]
    # for -x=autoinit
    initDepsNow, initDepsAtLast: seq[XfrpNodeId]
    delay: int
    case isInput: bool
    of true:
      inputType: WithCodeInfo[XfrpType]

    of false:
      innerTypeOpt: Option[WithCodeInfo[XfrpType]]
      update: WithCodeInfo[XfrpExpr]
      depsNow, depsAtLast: seq[XfrpNodeId]

  XfrpNodeDescriptionTable = TableRef[XfrpNodeId, XfrpNodeDescription]

  XfrpNodeEnv* = object
    ## A node environment.
    tbl: XfrpNodeDescriptionTable
    sortedInnerNodeIds: seq[XfrpNodeId]
    inputNodeIds, outputNodeIds: seq[XfrpNodeId]
    sortedInitNodeIds: seq[XfrpNodeId]
    # for -x=autoinit
    maxDelay: int

  XfrpNodeDependencyInfo = tuple
    depsNow, depsAtLast: seq[XfrpNodeId]


proc extractNodeDeps(nodeTbl: XfrpNodeDescriptionTable; exp: WithCodeInfo[XfrpExpr]; flags: set[CompilerFlag] = {}): XfrpNodeDependencyInfo =
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
          if (flagAutoInitExt notin flags) and node.initOpt.isNone:
            let err = XfrpReferenceError.newException("The node '" & id & "' has no initial value althought its at-last value is referred.")
            err.causedBy(node.id, exp)
            raise err

          result.depsAtLast.add id

    ExprBin(_, termAsts):
      let (depsNow, depsAtLast) = termAsts.mapIt(nodeTbl.extractNodeDeps(it, flags)).unzip()
      result.depsNow = deduplicate(concat(depsNow))
      result.depsAtLast = deduplicate(concat(depsAtLast))

    ExprIf(ifAstRef, thenAstRef, elseAstRef):
      let
        (ifDepsNow, ifDepsAtLast) = nodeTbl.extractNodeDeps(ifAstRef[], flags)
        (thenDepsNow, thenDepsAtLast) = nodeTbl.extractNodeDeps(thenAstRef[], flags)
        (elseDepsnow, elseDepsAtLast) = nodeTbl.extractNodeDeps(elseAstRef[], flags)

      result.depsNow = deduplicate(ifDepsNow & thenDepsNow & elseDepsNow)
      result.depsAtLast = deduplicate(ifDepsAtLast & thenDepsAtLast & elseDepsAtLast)

    ExprApp(_, argAsts):
      let (depsNow, depsAtLast) = argAsts.mapIt(nodeTbl.extractNodeDeps(it, flags)).unzip()

      result.depsNow = deduplicate(concat(depsNow))
      result.depsAtLast = deduplicate(concat(depsAtLast))

    ExprMagic(_, argAsts):
      let (depsNow, depsAtLast) = argAsts.mapIt(nodeTbl.extractNodeDeps(it, flags)).unzip()

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


proc getTopologicallySortedInitList(nodeTbl: XfrpNodeDescriptionTable): seq[XfrpNodeId] =
  ## Return node ID list by topologically-sorted ordering.
  func referencesOf(n: XfrpNodeId): seq[XfrpNodeId] =
    let node = nodeTbl[n]
    result = node.initDepsNow & node.initDepsAtLast

  let graph: ReferenceGraph[XfrpNodeId] =
    (domain: toSeq(keys(nodeTbl)), referencesOf: referencesOf)

  let referenceCycle = graph.getAnyReferenceCycle
  if referenceCycle.len > 0:
    let
      referenceCycleDiagram = (referenceCycle & referenceCycle[0]).join(" -> ")
      err = XfrpReferenceError.newException("A cycle initialization reference is detected. Explicit initialization expression is useful for avoiding such a cycle. (" & referenceCycleDiagram & ")")

    for nodeId in referenceCycle:
      err.causedBy(nodeTbl[nodeId].id)

    raise err

  result = graph.topologicallySorted()


proc makeNodeEnvironment*(materialTbl: XfrpMaterials; opEnv: XfrpOpEnv; flags: set[CompilerFlag] = {}): XfrpNodeEnv =
  ## Construct new node environment.
  let
    ast = materialTbl.getRoot().val
    nodeTbl = newTable[XfrpNodeId, XfrpNodeDescription]()
  var
    initTbl = initTable[XfrpNodeId, WithCodeInfo[XfrpExpr]]()
    inputNodeIds = newSeq[XfrpNodeId]()

  for inputNodeAst in ast.ins:
    let
      (idAst, tyAst, initAstOpt) = inputNodeAst.val.split()
      id = idAst.val

    if id in nodeTbl:
      let err = XfrpDefinitionError.newException("Redeclaration of an input node '" & id & "' is detected.")
      err.causedBy(idAst)
      raise err

    if initAstOpt.isSome:
      initTbl[id] = initAstOpt.get()

    nodeTbl[id] = XfrpNodeDescription(id: idAst, initOpt: initAstOpt, isInput: true, inputType: tyAst)
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

        if initAstOpt.isSome:
          if id in initTbl:
            let
              conflicted = initTbl[id]
              err = XfrpDefinitionError.newException("Initialization expression of node '" & id & "' is already defined.")
            err.causedBy(conflicted, def)
            raise err

          initTbl[id] = initAstOpt.get()

        nodeTbl[id] = XfrpNodeDescription(id: idAst, initOpt: initAstOpt, isInput: false,
          innerTypeOpt: typeAstOpt, update: opEnv.reparseBinaryExpression(bodyAst, ast.moduleId.val, materialTbl))

      DefInit(idAst, bodyAst):
        assert(flagAutoInitExt in flags)
        let id = idAst.val

        if id in initTbl:
          let
            conflicted = initTbl[id]
            err = XfrpDefinitionError.newException("Initialization expression of node '" & id & "' is already defined.")
          err.causedBy(conflicted, def)
          raise err

        initTbl[id] = opEnv.reparseBinaryExpression(bodyAst, ast.moduleId.val, materialTbl)

      _:
        discard

  if flagAutoInitExt in flags:
    for id, initBody in initTbl:
      if id notin nodeTbl:
        let err = XfrpDefinitionError.newException("Node '" & id & "' is not defined.")
        err.causedBy(initBody)
        raise err

      nodeTbl[id].initOpt = some(initBody)

    for nodeDesc in mvalues(nodeTbl):
      if not nodeDesc.isInput and nodeDesc.initOpt.isNone:
        nodeDesc.initOpt = some(nodeDesc.update)

  var outputNodeIds = newSeq[XfrpId]()
  for outputNode in ast.outs:
    let
      (idAst, tyAstOpt) = outputNode.val.split()
      id = idAst.val

    if id notin nodeTbl:
      let err = XfrpReferenceError.newException("Node '" & id & "' is not defined as an inner node.")
      err.causedBy(outputNode)
      raise err

    let node = nodeTbl[id]

    if node.isInput:
      let err = XfrpReferenceError.newException("Directly outputting any input node is prohibited.")
      err.causedBy(outputNode)
      raise err

    if tyAstOpt.isSome:
      if node.innerTypeOpt.isSome:
        let
          innerNodeTyAst = node.innerTypeOpt.unsafeGet()
          outputNodeTyAst = tyAstOpt.unsafeGet()
        if innerNodeTyAst.val != outputNodeTyAst.val:
          let err = XfrpTypeError.newException("An output node '" & id & "' has different type annotations.")
          err.causedBy(outputNodeTyAst, innerNodeTyAst)
          raise err

      else:
        nodeTbl[id].innerTypeOpt = tyAstOpt

    outputNodeIds.add id

  # Extract inner nodes' dependencies.
  for nodeDesc in mvalues(nodeTbl):
    if not nodeDesc.isInput:
      (nodeDesc.depsNow, nodeDesc.depsAtLast) = nodeTbl.extractNodeDeps(nodeDesc.update, flags)

    if (flagAutoInitExt in flags) and nodeDesc.initOpt.isSome:
      (nodeDesc.initDepsNow, nodeDesc.initDepsAtLast) = nodeTbl.extractNodeDeps(nodeDesc.initOpt.get(), flags)

  let sortedInnerNodeIds = getTopologicallySortedNodeList(nodeTbl)

  if flagAutoInitExt notin flags:
    result = XfrpNodeEnv(tbl: nodeTbl, sortedInnerNodeIds: sortedInnerNodeIds, inputNodeIds: inputNodeIds, outputNodeIds: outputNodeIds, maxDelay: 0)

  else:
    let sortedInitNodeIds = getTopologicallySortedInitList(nodeTbl)

    for nodeId in reversed(sortedInitNodeIds):
      let node = nodeTbl[nodeId]
      for initDepNodeId in node.initDepsNow:
        let initDepNode = nodeTbl[initDepNodeId]
        nodeTbl[initDepNodeId].delay = max(initDepNode.delay, node.delay)

      for initDepNodeId in node.initDepsAtLast:
        let initDepNode = nodeTbl[initDepNodeId]
        nodeTbl[initDepNodeId].delay = max(initDepNode.delay, succ(node.delay))

    let maxDelay = toSeq(values(nodeTbl)).mapIt(it.delay).max()

    result = XfrpNodeEnv(tbl: nodeTbl, sortedInnerNodeIds: sortedInnerNodeIds, sortedInitNodeIds: sortedInitNodeIds, inputNodeIds: inputNodeIds, outputNodeIds: outputNodeIds, maxDelay: maxDelay)


proc getNode*(env: XfrpNodeEnv; id: XfrpNodeId): XfrpNodeDescription =
  ## Get a node description by node ID.
  result = env.tbl[id]


iterator items*(env: XfrpNodeEnv): XfrpNodeId =
  ## Iterate all nodes by ID.
  for id in env.inputNodeIds:
    yield id

  for id in env.sortedInnerNodeIds:
    yield id


iterator innerNodeIds*(env: XfrpNodeEnv): XfrpNodeId =
  ## Iterate all inner nodes by ID.
  for id in env.sortedInnerNodeIds:
    yield id


iterator inputNodeIds*(env: XfrpNodeEnv): XfrpNodeId =
  ## Iterate all input nodes by ID.
  for id in env.inputNodeIds:
    yield id


iterator outputNodeIds*(env: XfrpNodeEnv): XfrpNodeId =
  ## Iterate all output nodes by ID.
  for id in env.outputNodeIds:
    yield id


iterator initNodesOfDelay*(env: XfrpNodeEnv; delay: int): XfrpNodeId =
  ## Iterate nodes whose delay is at least ``minDelay``.
  for id in env.sortedInitNodeIds:
    let node = env.tbl[id]
    if node.initOpt.isNone: continue
    elif node.delay == delay: yield id

# getters

func id*(desc: XfrpNodeDescription): WithCodeInfo[XfrpId] = desc.id
func initOpt*(desc: XfrpNodeDescription): Option[WithCodeInfo[XfrpExpr]] = desc.initOpt
func isInput*(desc: XfrpNodeDescription): bool = desc.isInput
func inputType*(desc: XfrpNodeDescription): WithCodeInfo[XfrpType] = desc.inputType
func innerTypeOpt*(desc: XfrpNodeDescription): Option[WithCodeInfo[XfrpType]] = desc.innerTypeOpt
func update*(desc: XfrpNodeDescription): WithCodeInfo[XfrpExpr] = desc.update

func maxDelay*(env: XfrpNodeEnv): int = env.maxDelay

iterator exprs*(env: XfrpNodeEnv): WithCodeInfo[XfrpExpr] =
  ## Iterate all expressions associated with nodes as update expressions or initialization expression.
  for desc in values(env.tbl):
    if not desc.isInput:
      yield desc.update

    if desc.initOpt.isSome:
      yield desc.initOpt.get()


iterator depsNow*(desc: XfrpNodeDescription): XfrpNodeId =
  ## Iterate all current-value dependencies of a given node.
  assert(not desc.isInput)
  for id in desc.depsNow:
    yield id

iterator depsAtLast*(desc: XfrpNodeDescription): XfrpNodeId =
  ## Iterate all at-last dependencies of a given node.
  assert(not desc.isInput)
  for id in desc.depsAtLast:
    yield id

iterator initDepsNow*(desc: XfrpNodeDescription): XfrpNodeId =
  ## Iterate all current-value dependencies of a given node while its initialization.
  for id in desc.initDepsNow:
    yield id

iterator initDepsAtLast*(desc: XfrpNodeDescription): XfrpNodeId =
  ## Iterate all at-last dependencies of a given node while its initialization.
  for id in desc.initDepsAtLast:
    yield id
