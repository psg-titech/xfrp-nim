import tables, options
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

  XfrpEnv* = object
    name: WithCodeInfo[XfrpModuleId]
    materials: TableRef[XfrpModuleId, XfrpEnv]
    funcs: TableRef[XfrpId, XfrpFuncDefinition]
    innerNodes: TableRef[XfrpId, XfrpNodeDefinition]
    inputNodes: OrderedTableRef[XfrpId, WithCodeInfo[XfrpIdAndType]]
    outputNodes: seq[WithCodeInfo[XfrpIdAndTypeOpt]]

proc makeEnvironmentFromModule*(ast: XfrpModule): XfrpEnv =
  # Initialization
  result.name = ast.moduleId
  # result.materials = newTable[XfrpId, XfrpEnv]()
  result.funcs = newTable[XfrpId, XfrpFuncDefinition]()
  result.innerNodes = newTable[XfrpId, XfrpNodeDefinition]()
  result.inputNodes = newOrderedTable[XfrpId, WithCodeInfo[XfrpIdAndType]]()
  result.outputNodes = ast.outs

  for node in ast.ins:
    let
      (idAst, _) = node.val
      id = idAst.val

    if id in result.inputNodes:
      let
        conflictedNode = result.inputNodes[id]
        err = XfrpDefinitionError.newException("Redeclaration of an input node '" & id & "' is detected.")
      err.causedBy(conflictedNode.val.id, idAst)
      raise err

    result.inputNodes[id] = node

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
          err.causedBy(idAst, conflictedNode.val.id)
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

  except XfrpLanguageError as err:
    stderr.writeLine "[Language Error] ", err.msg
