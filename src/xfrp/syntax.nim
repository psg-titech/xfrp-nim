import options
import patty
import types, codeinfos

# AST with code informations

type
  XfrpAst*[T] = WithCodeInfo[T]

func ast*[T](t: XfrpAst[T]): T = t.val

# Raw XFRP ASTs

type
  XfrpId* = string
  XfrpModuleId* = string

variantp XfrpIdAndType:
  IdWithExplicitType(idExplicit: XfrpAst[XfrpId], tyExplicit: XfrpAst[XfrpType])
  IdWithoutAnyTypeAnnot(idImplicit: XfrpAst[XfrpId])

variantp XfrpAnnotation:
  AnnotAtLast

variantp XfrpLiteral:
  LitUint
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
    binRte = ">="
    binRt = ">"

variantp XfrpExpr:
  ExprLiteral(constVal: XfrpAst[XfrpLiteral])
  ExprId(id: XfrpAst[XfrpId])
  ExprAnnot(annotId: XfrpAst[XfrpId], annot: XfrpAst[XfrpAnnotation])
  ExprBin(binOp: XfrpAst[XfrpBinOp], binLhs, binRhs: ref XfrpAst[XfrpExpr])
  ExprIf(ifExpr, thenExpr, elseExpr: ref XfrpAst[XfrpExpr])
  ExprApp(appId: XfrpAst[XfrpId], appArgs: seq[XfrpAst[XfrpExpr]])

variantp XfrpDefinition:
  DefNode(nodeIdAndType: XfrpAst[XfrpIdAndType], nodeInit: Option[XfrpAst[XfrpExpr]], nodeBody: XfrpAst[XfrpExpr])
  # DefConst(constIdAndType: XfrpIdAndType, constBody: XfrpExpr)
  DefFunc(funId: XfrpAst[XfrpId], funRetType: XfrpAst[XfrpType], funArgs: seq[XfrpAst[XfrpIdAndType]], funBody: XfrpAst[XfrpExpr])

type
  XfrpModule* = tuple
    moduleId: XfrpAst[XfrpModuleId]
    ins: seq[XfrpAst[XfrpIdAndType]]
    outs: seq[XfrpAst[XfrpIdAndType]]
    uses: seq[XfrpAst[XfrpModuleId]]
    defs: seq[XfrpAst[XfrpDefinition]]

proc id*(idAndType: XfrpIdAndType): XfrpId =
  match idAndType:
    IdWithExplicitType(idAst, _):
      return idAst.ast

    IdWithoutAnyTypeAnnot(idAst):
      return idAst.ast
