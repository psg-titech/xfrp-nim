import options
from strutils import parseInt, parseFloat
import nimly

import tokens, syntax, types

type
  XfrpParseError* = object of CatchableError

func ignores*(tk: XfrpToken): bool =
  tk.kind in {XfrpTokenKind.Ignore, XfrpTokenKind.Comment}

proc `~`[T](x: T): ref T =
  new result
  result[] = x

nimy xfrpParser[XfrpToken]:
  progModule[XfrpAst]:
    Module Id In idAndTypes Out idAndTypes definitions:
      return (($2).idStr, $4, $6, @[], $7)

    Module Id In idAndTypes Out idAndTypes useModules definitions:
      return (($2).idStr, $4, $6, $7, $8)

  idAndType[XfrpIdAndType]:
    Id Colon typeSpecific:
      return IdWithExplicitType(($1).idStr, $3)

  idAndTypes[seq[XfrpIdAndType]]:
    idAndType:
      return @[$1]

    idAndType Comma idAndTypes:
      return $1 & $3

  typeSpecific[XfrpType]:
    primTypeSpecific:
      return $1

  primTypeSpecific[XfrpType]:
    Id:
      case ($1).idStr
      of "Unit": return TUnit()
      of "Bool": return TBool()
      of "Int": return TInt()
      of "Float": return TFloat()
      else: raise XfrpParseError.newException("Type error: Not defined")

  useModules[seq[XfrpModuleId]]:
    Use modules:
      return $2

  modules[seq[XfrpModuleId]]:
    Id:
      return @[($1).idStr]

    Id Comma modules:
      return ($1).idStr & $3

  definitions[seq[XfrpDefinition]]:
    definition:
      return @[$1]

    definition definitions:
      return $1 & $2

  definition[XfrpDefinition]:
    Node idAndType Equal expression:
      return DefNode($2, none(XfrpExpr), $4)

    Node nodeInitDef idAndType Equal expression:
      return DefNode($3, some($2), $5)

    Function Id LParen idAndTypes RParen Colon typeSpecific Equal expression:
      return DefFunc(($2).idStr, $7, $4, $9)

  expression[XfrpExpr]:
    constant:
      return ExprConst($1)

    Id:
      return ExprId(($1).idStr)

    Id At annotation:
      return ExprAnnot(($1).idStr, $3)

    expression binOp expression:
      return ExprBin($2, ~($1), ~($3))

    If expression Then expression Else expression:
      return ExprIf(~($2), ~($4), ~($6))

    LParen expression RParen:
      return $2

    Id LParen appArguments RParen:
      return ExprApp(($1).idStr, $3)

  annotation[XfrpAnnotation]:
    Last:
      return AnnotAtLast()

  appArguments[seq[ref XfrpExpr]]:
    expression:
      return @[~($1)]

    expression Comma appArguments:
      return ~($1) & $3

  nodeInitDef[XfrpExpr]:
    Init LBracket nodeInitExpr RBracket:
      return $3

  nodeInitExpr[XfrpExpr]:
    constant:
      return ExprConst($1)

  binOp[XfrpBinOp]:
    Plus:
      return binAdd
    EqEq:
      return binEqEq
    VertVert:
      return binVertVert
    Lte:
      return binLte
    Lt:
      return binLt
    Rte:
      return binRte
    Rt:
      return binRt

  constant[XfrpConst]:
    True:
      return CBool(true)
    False:
      return CBool(false)
    Digits:
      return CInt(($1).intStr.parseInt())
    FDigits:
      return CFloat(($1).floatStr.parseFloat())

when isMainModule:
  import os
  import lexer

  if paramCount() < 1:
    echo "Usage: parser [filename]"
    quit QuitFailure

  var l = xfrpLex.open(paramStr(1))
  l.ignoreIf = ignores

  var p = xfrpParser.newParser()
  echo p.parse(l)
