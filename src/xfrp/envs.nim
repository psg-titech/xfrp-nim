# XFRP name environments.

import tables, options
from sequtils import deduplicate, mapIt, unzip, concat, toSeq
from strutils import join
import patty
import syntax, types, codeinfos, errors, topsort

export syntax.XfrpId

type
  XfrpFuncDefinition* = object
    id: WithCodeInfo[XfrpId]
    retType: WithCodeInfo[XfrpType]
    args: seq[WithCodeInfo[XfrpIdAndType]]
    body: WithCodeInfo[XfrpExpr]
    # additional info.
    deps: seq[XfrpId]

  XfrpNodeDefinition* = object
    id: WithCodeInfo[XfrpId]
    typeOpt: Option[WithCodeInfo[XfrpType]]
    init: Option[WithCodeInfo[XfrpExpr]]
    update: WithCodeInfo[XfrpExpr]
    # additional info.
    refNow, refAtLast: seq[XfrpId]

  XfrpInputNodeDefinition* = object
    id: WithCodeInfo[XfrpId]
    ty: WithCodeInfo[XfrpType]
    init: Option[WithCodeInfo[XfrpExpr]]

  XfrpEnv* = object
    name: WithCodeInfo[XfrpModuleId]
    materials: TableRef[XfrpModuleId, ref XfrpEnv]
    funcs: TableRef[XfrpId, XfrpFuncDefinition]
    innerNodes: TableRef[XfrpId, XfrpNodeDefinition]
    inputNodes: OrderedTableRef[XfrpId, XfrpInputNodeDefinition]
    outputNodes: seq[WithCodeInfo[XfrpIdAndTypeOpt]]
    # topologically sorted IDs
    sortedInnerNodeIds: seq[XfrpId]
    sortedFuncIds: seq[XfrpId]

  NodeReferenceInfo = tuple
    refNow, refAtLast: seq[XfrpId]


func extractNodeReferenceInfo(env: XfrpEnv; exp: WithCodeInfo[XfrpExpr]): NodeReferenceInfo =
  match exp.val:
    ExprLiteral(_):
      return

    ExprId(idAst):
      let id = idAst.val
      if (id in env.inputNodes) or (id in env.innerNodes):
        result.refNow = @[id]

      else:
        let err = XfrpReferenceError.newException("Variable '" & id & "' is not declared.")
        err.causedBy(exp)
        raise err

    ExprAnnot(idAst, annotAst):
      let id = idAst.val
      match annotAst:
        AnnotAtLast:
          if id in env.inputNodes:
            let inputNode = env.inputNodes[id]

            # check the existence of initial value for the input node
            if inputNode.init.isNone:
              let err = XfrpReferenceError.newException("The input node '" & id & "' has no initial value although its at-last value is referred.")
              err.causedBy(inputNode.id, exp)
              raise err

            result.refAtLast = @[id]

          elif id in env.innerNodes:
            let innerNode = env.innerNodes[id]

            # check the existence of initial value for the inner node
            if innerNode.init.isNone:
              let err = XfrpReferenceError.newException("The node '" & id & "' has no initial value although its at-last value is referred.")
              err.causedBy(innerNode.id, exp)
              raise err

            result.refAtLast = @[id]

          else:
            let err = XfrpReferenceError.newException("At-last reference is only for nodes, but '" & id & "' is not a node.")
            err.causedBy(exp)
            raise err

    ExprBin(_, lhsAst, rhsAst):
      let
        (lhsRefNow, lhsRefAtLast) = env.extractNodeReferenceInfo(lhsAst[])
        (rhsRefNow, rhsRefAtLast) = env.extractNodeReferenceInfo(rhsAst[])

      result.refNow = deduplicate(lhsRefnow & rhsRefNow)
      result.refAtLast = deduplicate(lhsRefAtLast & rhsRefAtLast)

    ExprIf(ifAst, thenAst, elseAst):
      let
        (ifRefNow, ifRefAtLast) = env.extractNodeReferenceInfo(ifAst[])
        (thenRefNow, thenRefAtLast) = env.extractNodeReferenceInfo(thenAst[])
        (elseRefNow, elseRefAtLast) = env.extractNodeReferenceInfo(elseAst[])

      result.refNow = deduplicate(ifRefNow & thenRefNow & elseRefNow)
      result.refAtLast = deduplicate(ifRefAtLast & thenRefAtLast & elseRefAtLast)

    ExprApp(idAst, argAsts):
      let id = idAst.val
      if id notin env.funcs:
        let err = XfrpReferenceError.newException("Function '" & id & "' is not declared.")
        err.causedBy(idAst)
        raise err

      let (refsNow, refsAtLast) = argAsts.mapIt(env.extractNodeReferenceInfo(it)).unzip()
      result.refNow = deduplicate(concat(refsNow))
      result.refAtLast = deduplicate(concat(refsAtLast))


func extractFuncDependencyInfo(env: XfrpEnv; exp: WithCodeInfo[XfrpExpr]): seq[XfrpId] =
  match exp.val:
    ExprLiteral(_): return
    ExprId(_):
      # constant variables are also dependencies
      return
    ExprAnnot(_, _): return
    ExprBin(_, lhsAst, rhsAst):
      let
        lhsDeps = env.extractFuncDependencyInfo(lhsAst[])
        rhsDeps = env.extractFuncDependencyInfo(rhsAst[])

      result = deduplicate(lhsDeps & rhsDeps)

    ExprIf(ifAst, thenAst, elseAst):
      let
        ifDeps = env.extractFuncDependencyInfo(ifAst[])
        thenDeps = env.extractFuncDependencyInfo(thenAst[])
        elseDeps = env.extractFuncDependencyInfo(elseAst[])

      result = deduplicate(ifDeps & thenDeps & elseDeps)

    ExprApp(idAst, argAsts):
      let id = idAst.val
      if id notin env.funcs:
        let err = XfrpReferenceError.newException("Function '" & id & "' is not declared.")
        err.causedBy(idAst)
        raise err

      let argDeps = argAsts.mapIt(env.extractFuncDependencyInfo(it)).concat()
      result = deduplicate(id & argDeps)


proc getTopologicallySortedNodeList(env: XfrpEnv): seq[XfrpId] =
  ## Return node ID list by topologically-sorterd ordering.
  func referencesOf(n: XfrpId): seq[XfrpId] =
    env.innerNodes[n].refNow

  let graph: ReferenceGraph[XfrpId] = (domain: toSeq(keys(env.innerNodes)), referencesOf: referencesOf)

  # Before sorting, check the existence of any reference cycle.
  let referenceCycle = graph.getAnyReferenceCycle()
  if referenceCycle.len > 0:
    let
      referenceCycleDiagram = (referenceCycle & referenceCycle[0]).join(" -> ")
      err = XfrpReferenceError.newException("A cycle reference is detected. At-last reference is useful for avoiding such a cycle reference. (" & referenceCycleDiagram & ")")
    for nodeId in referenceCycle:
      err.causedBy(env.innerNodes[nodeId].id)
    raise err

  # Sorting main
  result = graph.topologicallySorted(toSeq(keys(env.inputNodes)))

proc getTopologicallySortedFuncList(env: XfrpEnv): seq[XfrpId] =
  ## Return function ID list by topologically-sorterd ordering.
  func referencesOf(n: XfrpId): seq[XfrpId] =
    env.funcs[n].deps

  let graph: ReferenceGraph[XfrpId] = (domain: toSeq(keys(env.funcs)), referencesOf: referencesOf)

  # Before sorting, check the existence of any reference cycle.
  let referenceCycle = graph.getAnyReferenceCycle()
  if referenceCycle.len > 0:
    let
      referenceCycleDiagram = (referenceCycle & referenceCycle[0]).join(" -> ")
      err = XfrpReferenceError.newException("Definition of any recursive functions is prohibited. (" & referenceCycleDiagram & ")")
    for funcId in referenceCycle:
      err.causedBy(env.funcs[funcId].id)
    raise err

  # Sorting main
  result = graph.topologicallySorted()


proc makeEnvironmentFromModule*(ast: XfrpModule): XfrpEnv =
  # Initialization
  result.name = ast.moduleId
  # result.materials = newTable[XfrpId, ref XfrpEnv]()
  result.funcs = newTable[XfrpId, XfrpFuncDefinition]()
  result.innerNodes = newTable[XfrpId, XfrpNodeDefinition]()
  result.inputNodes = newOrderedTable[XfrpId, XfrpInputNodeDefinition]()
  result.outputNodes = ast.outs

  # Registration of input nodes
  for node in ast.ins:
    let
      (idAst, tyAst, initAstOpt) = node.val.split()
      id = idAst.val

    if id in result.inputNodes:
      let
        conflictedNode = result.inputNodes[id]
        err = XfrpDefinitionError.newException("Redeclaration of an input node '" & id & "' is detected.")
      err.causedBy(conflictedNode.id, idAst)
      raise err

    result.inputNodes[id] = XfrpInputNodeDefinition(id: idAst, ty: tyAst, init: initAstOpt)

  # Registration of inner nodes and functions
  for def in ast.defs:
    match def.val:
      DefNode(idAndTypeOptAst, initAstOpt, bodyAst):
        let
          (idAst, typeAstOpt) = split(idAndTypeOptAst.val)
          id = idAst.val

        if id in result.inputNodes:
          let
            conflictedNode = result.inputNodes[id]
            err = XfrpDefinitionError.newException("Node '" & id & "' is already defined as an input node.")
          err.causedBy(idAst, conflictedNode.id)
          raise err

        if id in result.innerNodes:
          let
            conflictedNodeDef = result.innerNodes[id]
            err = XfrpDefinitionError.newException("Node '" & id & "' is already defined.")
          err.causedBy(conflictedNodeDef.id, idAst)
          raise err

        result.innerNodes[id] = XfrpNodeDefinition(id: idAst, typeOpt: typeAstOpt, init: initAstOpt, update: bodyAst)

      DefFunc(idAst, retTypeAst, argAsts, bodyAst):
        let id = idAst.val

        if id in result.funcs:
          let
            conflictedFuncDef = result.funcs[id]
            err = XfrpDefinitionError.newException("Function '" & id & "' is already defined.")
          err.causedBy(conflictedFuncDef.id, idAst)
          raise err

        result.funcs[id] = XfrpFuncDefinition(id: idAst, retType: retTypeAst, args: argAsts, body: bodyAst)

  # Check validation of output nodes
  for outAst in result.outputNodes:
    let (idAst, _) = outAst.val.split()
    if idAst.val in result.inputNodes:
      let err = XfrpReferenceError.newException("Outputting an input node '" & idAst.val & "' is prohibited.")
      err.causedBy(idAst)
      raise err

    elif idAst.val notin result.innerNodes:
      let err = XfrpReferenceError.newException("Output node '" & idAst.val & "' is not declared.")
      err.causedBy(idAst)
      raise err

  # Extract node references
  for nodeDef in mvalues(result.innerNodes):
    (nodeDef.refNow, nodeDef.refAtLast) = result.extractNodeReferenceInfo(nodeDef.update)

  # Extract function dependencies
  for funcDef in mvalues(result.funcs):
    funcDef.deps = result.extractFuncDependencyInfo(funcDef.body)

  # Topologically sorting
  result.sortedInnerNodeIds = result.getTopologicallySortedNodeList()
  result.sortedFuncIds = result.getTopologicallySortedFuncList()


func name*(env: XfrpEnv): WithCodeInfo[XfrpModuleId] = env.name

func getInnerNode*(env: XfrpEnv; id: XfrpId): XfrpNodeDefinition =
  env.innerNodes[id]

func getInputNode*(env: XfrpEnv; id: XfrpId): XfrpInputNodeDefinition =
  env.inputNodes[id]

func getFunction*(env: XfrpEnv; id: XfrpId): XfrpFuncDefinition =
  env.funcs[id]

func isInputNode*(env: XfrpEnv; id: XfrpId): bool =
  id in env.inputNodes

iterator innerNodeIds*(env: XfrpEnv): XfrpId =
  ## Iterate inner node ID by topologically-sorted ordering
  for node in env.sortedInnerNodeIds:
    yield node

iterator inputNodeIds*(env: XfrpEnv): XfrpId =
  for node in keys(env.inputNodes):
    yield node

iterator nodeIds*(env: XfrpEnv): XfrpId =
  for node in inputNodeIds(env):
    yield node
  for node in innerNodeIds(env):
    yield node

iterator functionIds*(env: XfrpEnv): XfrpId =
  ## Iterate function ID by topologically-sorted ordering
  for f in env.sortedFuncIds:
    yield f

iterator outputNodeIdAndTypeOptAsts*(env: XfrpEnv): WithCodeInfo[XfrpIdAndTypeOpt] =
  for idAndTyOptAst in env.outputNodes:
    yield idAndTyOptAst

iterator outputNodeIds*(env: XfrpEnv): XfrpId =
  for idAndTyOptAst in env.outputNodes:
    let (idAst, _) = split(idAndTyOptAst.val)
    yield idAst.val

func id*(nodeDef: XfrpNodeDefinition): WithCodeInfo[XfrpId] = nodeDef.id
func typeOpt*(nodeDef: XfrpNodeDefinition): Option[WithCodeInfo[XfrpType]] = nodeDef.typeOpt
func init*(nodeDef: XfrpNodeDefinition): Option[WithCodeInfo[XfrpExpr]] = nodeDef.init
func update*(nodeDef: XfrpNodeDefinition): WithCodeInfo[XfrpExpr] = nodeDef.update
func refNow*(nodeDef: XfrpNodeDefinition): seq[XfrpId] = nodeDef.refNow
func refAtLast*(nodeDef: XfrpNodeDefinition): seq[XfrpId] = nodeDef.refAtLast

func id*(funcDef: XfrpFuncDefinition): WithCodeInfo[XfrpId] = funcDef.id
func retType*(funcDef: XfrpFuncDefinition): WithCodeInfo[XfrpType] = funcDef.retType
func args*(funcDef: XfrpFuncDefinition): seq[WithCodeInfo[XfrpIdAndType]] = funcDef.args
func body*(funcDef: XfrpFuncDefinition): WithCodeInfo[XfrpExpr] = funcDef.body
func deps*(funcDef: XfrpFuncDefinition): seq[XfrpId] = funcDef.deps

func id*(inputDef: XfrpInputNodeDefinition): WithCodeInfo[XfrpId] = inputDef.id
func ty*(inputDef: XfrpInputNodeDefinition): WithCodeInfo[XfrpType] = inputDef.ty
func init*(inputDef: XfrpInputNodeDefinition): Option[WithCodeInfo[XfrpExpr]] = inputDef.init

when isMainModule:
  import os, json, std/jsonutils
  import lexer, parser

  if paramCount() < 1:
    echo "Usage: envs [filename]"
    quit QuitFailure

  try:
    var l = buildLexerFromFilename(paramStr(1))
    let
      ast = parse(l)
      env = makeEnvironmentFromModule(ast.val)

    echo pretty(env.toJson())

  except XfrpTypeError as err:
    stderr.writeLine "[Type Error] ", err.msg
    for info in err.causes:
      stderr.writeLine pretty(info)

  except XfrpSyntaxError as err:
    stderr.writeLine "[Syntax Error] ", err.msg
    for info in err.causes:
      stderr.writeLine pretty(info)

  except XfrpDefinitionError as err:
    stderr.writeLine "[Definition Error] ", err.msg
    for info in err.causes:
      stderr.writeLine pretty(info)

  except XfrpReferenceError as err:
    stderr.writeLine "[Reference Error] ", err.msg
    for info in err.causes:
      stderr.writeLine pretty(info)

  except XfrpLanguageError as err:
    stderr.writeLine "[Language Error] ", err.msg
