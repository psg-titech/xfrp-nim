import tables, options
import patty
import syntax, types

type
  XfrpFuncDefinition* = object
    id: XfrpId
    retType: XfrpType
    args: seq[XfrpIdAndType]
    body: XfrpExpr

  XfrpNodeDefinition* = object
    idAndType: XfrpIdAndType
    init: Option[XfrpExpr]
    update: XfrpExpr

  XfrpEnv* = object
    materials: TableRef[XfrpModuleId, ref XfrpEnv]
    funcs: TableRef[XfrpId, XfrpFuncDefinition]
    innerNodes: TableRef[XfrpId, XfrpNodeDefinition]
    inputNodes: OrderedTableRef[XfrpId, XfrpType]
    outputNodes: seq[XfrpIdAndType]

proc makeEnvironmentFromAst*(ast: XfrpAst): XfrpEnv =
  # Initialization
  # result.materials = newTable[XfrpId, ref XfrpEnv]()
  result.funcs = newTable[XfrpId, XfrpFuncDefinition]()
  result.innerNodes = newTable[XfrpId, XfrpNodeDefinition]()
  result.inputNodes = newOrderedTable[XfrpId, XfrpType]()
  result.outputNodes = ast.outs

  for node in ast.ins:
    match node:
      IdWithExplicitType(id, ty):
        if id in result.inputNodes:
          # 二重定義
          discard

        result.inputNodes[id] = ty

      IdWithoutAnyTypeAnnot(_):
        # 入力ノードは型を示す必要がある
        discard

  for def in ast.defs:
    match def:
      DefNode(idAndType, init, body):
        let id = idAndType.id
        if id in result.innerNodes:
          # 二重定義
          discard

        if id in result.inputNodes:
          discard

        result.innerNodes[id] = XfrpNodeDefinition(idAndType: idAndType, init: init, update: body)

      DefFunc(id, retType, args, body):
        if id in result.funcs:
          # 二重定義
          discard

        result.funcs[id] = XfrpFuncDefinition(id: id, retType: retType, args: args, body: body)
