import tables, options
from sequtils import deduplicate, mapIt, unzip, concat
import patty
import syntax, types, codeinfos, errors

type
  XfrpFuncDefinition* = object
    id: WithCodeInfo[XfrpId]
    retType: WithCodeInfo[XfrpType]
    args: seq[WithCodeInfo[XfrpIdAndType]]
    body: WithCodeInfo[XfrpExpr]

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

    ExprApp(_, argAsts):
      let (refsNow, refsAtLast) = argAsts.mapIt(env.extractNodeReferenceInfo(it)).unzip()
      result.refNow = deduplicate(concat(refsNow))
      result.refAtLast = deduplicate(concat(refsAtLast))


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

  # Registration of inner nodes
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

  # Extract node references
  for nodeDef in mvalues(result.innerNodes):
    (nodeDef.refNow, nodeDef.refAtLast) = result.extractNodeReferenceInfo(nodeDef.update)


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
