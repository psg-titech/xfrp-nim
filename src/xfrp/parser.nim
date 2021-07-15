import options
from strutils import parseInt, parseFloat
import nimly

import tokens, syntax, types, codeinfos, errors

func ignores*(tk: XfrpToken): bool =
  tk.kind in {XfrpTokenKind.Ignore, XfrpTokenKind.Comment}

proc `~`[T](x: T): ref T =
  new result
  result[] = x

nimy xfrpParser[XfrpToken]:
  progModule[XfrpAst[XfrpModule]]:
    Module Id In idAndTypes Out idAndTypes definitions:
      return (($2).idStr from $2, $4, $6, newSeq[XfrpAst[XfrpModuleId]](), $7) from ($1)..($7)[^1]

    Module Id In idAndTypes Out idAndTypes useModules definitions:
      return (($2).idStr from $2, $4, $6, $7, $8) from ($1)..($8)[^1]

  idAndType[XfrpAst[XfrpIdAndType]]:
    Id Colon typeSpecific:
      return IdWithExplicitType(($1).idStr from $1, $3) from ($1)..($3)

  idAndTypes[seq[XfrpAst[XfrpIdAndType]]]:
    idAndType:
      return @[$1]

    idAndType Comma idAndTypes:
      return $1 & $3

  typeSpecific[XfrpAst[XfrpType]]:
    primTypeSpecific:
      return $1

  primTypeSpecific[XfrpAst[XfrpType]]:
    Id:
      case ($1).idStr
      of "Unit": return TUnit() from $1
      of "Bool": return TBool() from $1
      of "Int": return TInt() from $1
      of "Float": return TFloat() from $1
      else: raise XfrpSyntaxError.newException("Type error: Not defined")

  useModules[seq[XfrpAst[XfrpModuleId]]]:
    Use modules:
      return $2

  modules[seq[XfrpAst[XfrpModuleId]]]:
    Id:
      return @[($1).idStr from $1]

    Id Comma modules:
      return (($1).idStr from $1) & $3

  definitions[seq[XfrpAst[XfrpDefinition]]]:
    definition:
      return @[$1]

    definition definitions:
      return $1 & $2

  definition[XfrpAst[XfrpDefinition]]:
    Node idAndType Equal expression:
      return DefNode($2, none(XfrpAst[XfrpExpr]), $4) from ($1)..($4)

    Node nodeInitDef idAndType Equal expression:
      return DefNode($3, some($2), $5) from ($1)..($5)

    Function Id LParen idAndTypes RParen Colon typeSpecific Equal expression:
      return DefFunc(($2).idStr from $2, $7, $4, $9) from ($1)..($9)

  expression[XfrpAst[XfrpExpr]]:
    literal:
      return ExprLiteral($1) from $1

    Id:
      return ExprId(($1).idStr from $1) from $1

    Id At annotation:
      return ExprAnnot(($1).idStr from $1, $3) from ($1)..($3)

    expression binOp expression:
      return ExprBin($2, ~($1), ~($3)) from ($1)..($3)

    If expression Then expression Else expression:
      return ExprIf(~($2), ~($4), ~($6)) from ($1)..($6)

    LParen expression RParen:
      return $2

    Id LParen appArguments RParen:
      return ExprApp(($1).idStr from $1, $3) from ($1)..($4)

  annotation[XfrpAst[XfrpAnnotation]]:
    Last:
      return AnnotAtLast() from $1

  appArguments[seq[XfrpAst[XfrpExpr]]]:
    expression:
      return @[$1]

    expression Comma appArguments:
      return $1 & $3

  nodeInitDef[XfrpAst[XfrpExpr]]:
    Init LBracket nodeInitExpr RBracket:
      return ($3).ast from ($1)..($4)

  nodeInitExpr[XfrpAst[XfrpExpr]]:
    literal:
      return ExprLiteral($1) from $1

  binOp[XfrpAst[XfrpBinOp]]:
    Plus:
      return binAdd from $1
    EqEq:
      return binEqEq from $1
    VertVert:
      return binVertVert from $1
    Lte:
      return binLte from $1
    Lt:
      return binLt from $1
    Rte:
      return binRte from $1
    Rt:
      return binRt from $1

  literal[XfrpAst[XfrpLiteral]]:
    True:
      return LitBool(true) from $1
    False:
      return LitBool(false) from $1
    Digits:
      return LitInt(($1).intStr.parseInt()) from $1
    FDigits:
      return LitFloat(($1).floatStr.parseFloat()) from $1

when isMainModule:
  import os, json, std/jsonutils
  import lexer

  if paramCount() < 1:
    echo "Usage: parser [filename]"
    quit QuitFailure

  var l = xfrpLex.open(paramStr(1))
  l.ignoreIf = ignores

  var p = xfrpParser.newParser()
  echo pretty(p.parse(l).toJson())
