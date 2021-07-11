import options
import patty
import types

type
  XfrpId* = string
  XfrpModuleId* = string
  XfrpIdAndType* = tuple
    id: XfrpId
    ty: XfrpType
  XfrpIdAndTypeOpt* = tuple
    id: XfrpId
    tyOpt: Option[XfrpType]
  XfrpAnnotation* = enum
    atLast = "@last"

variantp XfrpConst:
  CUint
  CBool(boolVal: bool)
  CInt(intVal: int)
  CFloat(floatVal: float)

type
  XfrpBinOp* = enum
    binAdd = "+"
    binEqEq = "=="
    binVertVert = "||"
    binLte = "<="
    binLt = "<"
    binRte = ">="
    binRt = ">"

variantp XfrpExpr:
  ExprConst(constVal: XfrpConst)
  ExprId(id: XfrpId)
  ExprAnnot(annotId: XfrpId, annot: XfrpAnnotation)
  ExprBin(binOp: XfrpBinOp, binLhs, binRhs: ref XfrpExpr)
  ExprIf(ifExpr, thenExpr, elseExpr: ref XfrpExpr)
  ExprApp(appId: XfrpId, appArgs: seq[ref XfrpExpr])

variantp XfrpDefinition:
  DefNode(nodeIdAndType: XfrpIdAndType, nodeInit: Option[XfrpExpr], nodeBody: XfrpExpr)
  DefConst(constIdAndType: XfrpIdAndType, constBody: XfrpExpr)
  DefFunc(funId: XfrpId, funRetType: XfrpType, funArgIds: seq[XfrpId], funArgTypes: seq[XfrpType], funBody: XfrpExpr)

type
  XfrpAst* = tuple
    moduleId: XfrpModuleId
    ins: seq[XfrpIdAndType]
    outs: seq[XfrpIdAndType]
    uses: seq[XfrpModuleId]
    defs: seq[XfrpDefinition]
