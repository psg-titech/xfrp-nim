## XFRP abstract syntax trees (ASTs).

import options
import patty
import types, codeinfos

# Raw XFRP ASTs

type
  XfrpId* = string
  XfrpModuleId* = string

  XfrpIdAndType* = tuple
    id: WithCodeInfo[XfrpId]
    ty: WithCodeInfo[XfrpType]


variantp XfrpIdAndTypeOpt:
  IdWithExplicitType(idExplicit: WithCodeInfo[XfrpId], tyExplicit: WithCodeInfo[XfrpType])
  IdWithoutAnyTypeAnnot(idImplicit: WithCodeInfo[XfrpId])


variantp XfrpAnnotation:
  AnnotAtLast


variantp XfrpLiteral:
  LitBool(boolVal: bool)
  LitInt(intVal: int)
  LitFloat(floatVal: float)


type
  XfrpOperator* = string

  XfrpOperatorPrecedenceLevel* = Natural

  XfrpOperatorAssociativity* = enum
    assocLeft, assocRight, assocNone


variantp XfrpExpr:
  ExprLiteral(litVal: WithCodeInfo[XfrpLiteral])
  ExprId(id: WithCodeInfo[XfrpId])
  ExprAnnot(annotId: WithCodeInfo[XfrpId], annot: WithCodeInfo[XfrpAnnotation])
  ExprBin(binOps: seq[WithCodeInfo[XfrpOperator]], binTerms: seq[WithCodeInfo[XfrpExpr]])
  ExprIf(ifExpr, thenExpr, elseExpr: ref WithCodeInfo[XfrpExpr])
  ExprApp(appId: WithCodeInfo[XfrpId], appArgs: seq[WithCodeInfo[XfrpExpr]])
  ExprMagic(magicIdAndType: WithCodeInfo[XfrpIdAndType], magicArgs: seq[WithCodeInfo[XfrpExpr]])


variantp XfrpDefinition:
  DefNode(nodeIdAndTypeOpt: WithCodeInfo[XfrpIdAndTypeOpt], nodeInit: Option[WithCodeInfo[XfrpExpr]], nodeBody: WithCodeInfo[XfrpExpr])
  DefFunc(funId: WithCodeInfo[XfrpId], funRetType: WithCodeInfo[XfrpType], funArgs: seq[WithCodeInfo[XfrpIdAndType]], funBody: WithCodeInfo[XfrpExpr])
  DefOp(operator: WithCodeInfo[XfrpOperator], opRetType: WithCodeInfo[XfrpType], opArgs: seq[WithCodeInfo[XfrpIdAndType]], opBody: WithCodeInfo[XfrpExpr])
  DefInfix(infixOp: WithCodeInfo[XfrpOperator], infixLevel: XfrpOperatorPrecedenceLevel, infixAssoc: XfrpOperatorAssociativity)


variantp XfrpInput:
  InputWithoutInit(idAndTypeNoInit: WithCodeInfo[XfrpIdAndType])
  InputWithInit(idAndTypeWithInit: WithCodeInfo[XfrpIdAndType], init: WithCodeInfo[XfrpExpr])


type
  XfrpEmit* = tuple
    target, body: string

  XfrpModuleKind* = enum
    modModule
    modMaterial

  XfrpModule* = tuple
    ## Either module or material.
    kind: XfrpModuleKind
    moduleId: WithCodeInfo[XfrpModuleId]
    ins: seq[WithCodeInfo[XfrpInput]]
    outs: seq[WithCodeInfo[XfrpIdAndTypeOpt]]
    uses: seq[WithCodeInfo[XfrpModuleId]]
    emits: seq[WithCodeInfo[XfrpEmit]]
    defs: seq[WithCodeInfo[XfrpDefinition]]


proc makeXfrpModule*(moduleId: WithCodeInfo[XfrpModuleId]; ins: seq[WithCodeInfo[XfrpInput]];
    outs: seq[WithCodeInfo[XfrpIdAndTypeOpt]]; uses: seq[WithCodeInfo[XfrpModuleId]] = @[];
    emits: seq[WithCodeInfo[XfrpEmit]] = @[]; defs: seq[WithCodeInfo[XfrpDefinition]]): XfrpModule =
  result = (modModule, moduleId, ins, outs, uses, emits, defs)


proc makeXfrpMaterial*(moduleId: WithCodeInfo[XfrpModuleId]; uses: seq[WithCodeInfo[XfrpModuleId]] = @[];
    emits: seq[WithCodeInfo[XfrpEmit]] = @[]; defs: seq[WithCodeInfo[XfrpDefinition]]): XfrpModule =
  result = (modMaterial, moduleId, @[], @[], uses, emits, defs)


# Utilities

proc split*(idAndTypeOpt: XfrpIdAndTypeOpt): tuple[id: WithCodeInfo[XfrpId], typeOpt: Option[WithCodeInfo[XfrpType]]] =
  match idAndTypeOpt:
    IdWithExplicitType(idAst, tyAst):
      return (idAst, some(tyAst))

    IdWithoutAnyTypeAnnot(idAst):
      return (idAst, none(WithCodeInfo[XfrpType]))

proc split*(input: XfrpInput): tuple[id: WithCodeInfo[XfrpId], ty: WithCodeInfo[XfrpType], initOpt: Option[WithCodeInfo[XfrpExpr]]] =
  match input:
    InputWithoutInit(idAndTypeAst):
      let (idAst, tyAst) = idAndTypeAst.val
      return (idAst, tyAst, none(WithCodeInfo[XfrpExpr]))

    InputWithInit(idAndTypeAst, initAst):
      let (idAst, tyAst) = idAndTypeAst.val
      return (idAst, tyAst, some(initAst))
