## XFRP abstract syntax trees (ASTs).

import options
import patty
import types, codeinfos

# AST with code informations

type
  XfrpAst*[T] = WithCodeInfo[T]

# Raw XFRP ASTs

type
  XfrpId* = string
  XfrpModuleId* = string

  XfrpIdAndType* = tuple
    id: XfrpAst[XfrpId]
    ty: XfrpAst[XfrpType]


variantp XfrpIdAndTypeOpt:
  IdWithExplicitType(idExplicit: XfrpAst[XfrpId], tyExplicit: XfrpAst[XfrpType])
  IdWithoutAnyTypeAnnot(idImplicit: XfrpAst[XfrpId])


variantp XfrpAnnotation:
  AnnotAtLast


variantp XfrpLiteral:
  LitUnit
  LitBool(boolVal: bool)
  LitInt(intVal: int)
  LitFloat(floatVal: float)


type
  XfrpBinOp* = enum
    binAdd = "+"
    binEqEq = "=="
    binVertVert = "||"
    binLte = "<="
    binLt = "<"
    binGte = ">="
    binGt = ">"


variantp XfrpExpr:
  ExprLiteral(litVal: XfrpAst[XfrpLiteral])
  ExprId(id: XfrpAst[XfrpId])
  ExprAnnot(annotId: XfrpAst[XfrpId], annot: XfrpAst[XfrpAnnotation])
  ExprBin(binOp: XfrpAst[XfrpBinOp], binLhs, binRhs: ref XfrpAst[XfrpExpr])
  ExprIf(ifExpr, thenExpr, elseExpr: ref XfrpAst[XfrpExpr])
  ExprApp(appId: XfrpAst[XfrpId], appArgs: seq[XfrpAst[XfrpExpr]])


variantp XfrpDefinition:
  DefNode(nodeIdAndTypeOpt: XfrpAst[XfrpIdAndTypeOpt], nodeInit: Option[XfrpAst[XfrpExpr]], nodeBody: XfrpAst[XfrpExpr])
  # DefConst(constIdAndType: XfrpIdAndType, constBody: XfrpExpr)
  DefFunc(funId: XfrpAst[XfrpId], funRetType: XfrpAst[XfrpType], funArgs: seq[XfrpAst[XfrpIdAndType]], funBody: XfrpAst[XfrpExpr])


variantp XfrpInput:
  InputWithoutInit(idAndTypeNoInit: XfrpAst[XfrpIdAndType])
  InputWithInit(idAndTypeWithInit: XfrpAst[XfrpIdAndType], init: XfrpAst[XfrpExpr])


type
  XfrpModule* = tuple
    moduleId: XfrpAst[XfrpModuleId]
    ins: seq[XfrpAst[XfrpInput]]
    outs: seq[XfrpAst[XfrpIdAndTypeOpt]]
    uses: seq[XfrpAst[XfrpModuleId]]
    defs: seq[XfrpAst[XfrpDefinition]]


proc split*(idAndTypeOpt: XfrpIdAndTypeOpt): tuple[id: XfrpAst[XfrpId], typeOpt: Option[XfrpAst[XfrpType]]] =
  match idAndTypeOpt:
    IdWithExplicitType(idAst, tyAst):
      return (idAst, some(tyAst))

    IdWithoutAnyTypeAnnot(idAst):
      return (idAst, none(XfrpAst[XfrpType]))

proc split*(input: XfrpInput): tuple[id: XfrpAst[XfrpId], ty: XfrpAst[XfrpType], initOpt: Option[XfrpAst[XfrpExpr]]] =
  match input:
    InputWithoutInit(idAndTypeAst):
      let (idAst, tyAst) = idAndTypeAst.val
      return (idAst, tyAst, none(XfrpAst[XfrpExpr]))

    InputWithInit(idAndTypeAst, initAst):
      let (idAst, tyAst) = idAndTypeAst.val
      return (idAst, tyAst, some(initAst))
