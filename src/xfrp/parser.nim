## XFRP parser.

import options
from strutils import parseInt, parseFloat
import nimly

import tokens, syntax, types, codeinfos, errors
from lexer import XfrpLexer

export lexer.XfrpLexer

func ignores(tk: XfrpToken): bool =
  tk.kind in {XfrpTokenKind.Ignore, XfrpTokenKind.Comment}


proc `~`[T](x: T): ref T =
  new result
  result[] = x


nimy xfrpParser[XfrpToken]:
  progModule[XfrpAst[XfrpModule]]:
    Module Id In inputIdAndTypes Out idAndTypeOpts definitions:
      return (modModule, ($2).idStr from $2, $4, $6, newSeq[XfrpAst[XfrpModuleId]](), newSeq[XfrpAst[XfrpEmit]](), $7) from ($1)..($7)[^1]

    Module Id In inputIdAndTypes Out idAndTypeOpts emitStmts definitions:
      return (modModule, ($2).idStr from $2, $4, $6, newSeq[XfrpAst[XfrpModuleId]](), $7, $8) from ($1)..($8)[^1]

    Module Id In inputIdAndTypes Out idAndTypeOpts useModules definitions:
      return (modModule, ($2).idStr from $2, $4, $6, $7, newSeq[XfrpAst[XfrpEmit]](), $8) from ($1)..($8)[^1]

    Module Id In inputIdAndTypes Out idAndTypeOpts useModules emitStmts definitions:
      return (modModule, ($2).idStr from $2, $4, $6, $7, $8, $9) from ($1)..($9)[^1]

    Material Id useModules[] definitions:
      if ($3).len > 0:
        return (modMaterial, ($2).idStr from $2,newSeq[XfrpAst[XfrpInput]](), newSeq[XfrpAst[XfrpIdAndTypeOpt]](), ($3)[0], newSeq[XfrpAst[XfrpEmit]](), $4) from ($1)..($4)[^1]

      else:
        return (modMaterial, ($2).idStr from $2,newSeq[XfrpAst[XfrpInput]](), newSeq[XfrpAst[XfrpIdAndTypeOpt]](), newSeq[XfrpAst[XfrpModuleId]](), newSeq[XfrpAst[XfrpEmit]](), $4) from ($1)..($4)[^1]

    Material Id useModules[] emitStmts definitions:
      if ($3).len > 0:
        return (modMaterial, ($2).idStr from $2,newSeq[XfrpAst[XfrpInput]](), newSeq[XfrpAst[XfrpIdAndTypeOpt]](), ($3)[0], $4, $5) from ($1)..($5)[^1]

      else:
        return (modMaterial, ($2).idStr from $2,newSeq[XfrpAst[XfrpInput]](), newSeq[XfrpAst[XfrpIdAndTypeOpt]](), newSeq[XfrpAst[XfrpModuleId]](), $4, $5) from ($1)..($5)[^1]

  emitStmts[seq[XfrpAst[XfrpEmit]]]:
    emitStmt:
      return @[$1]

    emitStmt emitStmts:
      return ($1) & $2

  emitStmt[XfrpAst[XfrpEmit]]:
    Emit In Id TripleQuoted:
      return (($3).idStr, ($4).tqStr[3..^4]) from ($1)..($4)

  idAndType[XfrpAst[XfrpIdAndType]]:
    Id Colon typeSpecific:
      return (($1).idStr from $1, $3) from ($1)..($3)

  idAndTypes[seq[XfrpAst[XfrpIdAndType]]]:
    idAndType:
      return @[$1]

    idAndType Comma idAndTypes:
      return $1 & $3

  inputIdAndType[XfrpAst[XfrpInput]]:
    idAndType:
      return InputWithoutInit($1) from $1

    Id LParen nodeInitExpr RParen Colon typeSpecific:
      return InputWithInit((($1).idStr from $1, $6) from ($1)..($6), $3) from ($1)..($6)

  inputIdAndTypes[seq[XfrpAst[XfrpInput]]]:
    inputIdAndType:
      return @[$1]

    inputIdAndType Comma inputIdAndTypes:
      return $1 & $3

  idAndTypeOpt[XfrpAst[XfrpIdAndTypeOpt]]:
    Id Colon typeSpecific:
      return IdWithExplicitType(($1).idStr from $1, $3) from ($1)..($3)

    Id:
      return IdWithoutAnyTypeAnnot(($1).idStr from $1) from $1

  idAndTypeOpts[seq[XfrpAst[XfrpIdAndTypeOpt]]]:
    idAndTypeOpt:
      return @[$1]

    idAndTypeOpt Comma idAndTypeOpts:
      return $1 & $3

  typeSpecific[XfrpAst[XfrpType]]:
    primTypeSpecific:
      return $1

  primTypeSpecific[XfrpAst[XfrpType]]:
    Id:
      case ($1).idStr
      of "Bool": return TBool() from $1
      of "Int": return TInt() from $1
      of "Float": return TFloat() from $1
      else:
        let err = XfrpTypeError.newException("No such type is defined.")
        err.causedBy($1)
        raise err

  useModules[seq[XfrpAst[XfrpModuleId]]]:
    Use modules:
      return $2

  modules[seq[XfrpAst[XfrpModuleId]]]:
    moduleId:
      return @[$1]

    moduleId Comma modules:
      return ($1) & $3

  moduleId[XfrpAst[XfrpModuleId]]:
    Id:
      return ($1).idStr from $1

    Id Slash moduleId:
      return (($1).idStr & "/" & ($3).val) from ($1)..($3)

  definitions[seq[XfrpAst[XfrpDefinition]]]:
    definition:
      return @[$1]

    definition definitions:
      return $1 & $2

  definition[XfrpAst[XfrpDefinition]]:
    Node idAndTypeOpt Equal expression:
      return DefNode($2, none(XfrpAst[XfrpExpr]), $4) from ($1)..($4)

    Node nodeInitDef idAndTypeOpt Equal expression:
      return DefNode($3, some($2), $5) from ($1)..($5)

    Function Id LParen idAndTypes RParen Colon typeSpecific Equal expression:
      return DefFunc(($2).idStr from $2, $7, $4, $9) from ($1)..($9)

    Function operator LParen idAndTypes RParen Colon typeSpecific Equal expression:
      return DefOp($2, $7, $4, $9) from ($1)..($9)

  expression[XfrpAst[XfrpExpr]]:
    binaryExpression:
      return $1

    If expression Then expression Else expression:
      return ExprIf(~($2), ~($4), ~($6)) from ($1)..($6)

  binaryExpression[XfrpAst[XfrpExpr]]:
    primitiveExpression:
      return $1

    primitiveExpression operator binaryExpression:
      if ($3).val.kind == XfrpExprKind.ExprBin:
        return ExprBin(($2) & ($3).val.binOps, ($1) & ($3).val.binTerms) from ($1)..($3)
      else:
        return ExprBin(@[$2], @[$1, $3]) from ($1)..($3)

  primitiveExpression[XfrpAst[XfrpExpr]]:
    literal:
      return ExprLiteral($1) from $1

    Id:
      return ExprId(($1).idStr from $1) from $1

    Id At annotation:
      return ExprAnnot(($1).idStr from $1, $3) from ($1)..($3)

    LParen expression RParen:
      return $2

    Id LParen appArguments RParen:
      return ExprApp(($1).idStr from $1, $3) from ($1)..($4)

    Magic LParen idAndType RParen:
      return ExprMagic($3, @[]) from ($1)..($4)

    Magic LParen idAndType Comma appArguments RParen:
      return ExprMagic($3, $5) from ($1)..($6)

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
      return ($3).val from ($1)..($4)

  nodeInitExpr[XfrpAst[XfrpExpr]]:
    literal:
      return ExprLiteral($1) from $1

  operator[XfrpAst[XfrpOperator]]:
    Operator:
      return ($1).opStr from $1

    Slash:
      return "/" from $1

  literal[XfrpAst[XfrpLiteral]]:
    True:
      return LitBool(true) from $1
    False:
      return LitBool(false) from $1
    Digits:
      return LitInt(($1).intStr.parseInt()) from $1
    FDigits:
      return LitFloat(($1).floatStr.parseFloat()) from $1


proc parse*(l: var XfrpLexer): XfrpAst[XfrpModule] =
  l.ignoreIf = ignores

  var p = xfrpParser.newParser()

  try:
    result = p.parse(l)

  except NimlEOFError as err:
    var err0 = XfrpSyntaxError.newException("Unexpected EOF.", err)
    raise err0

  except LexError as err:
    var err0 = XfrpSyntaxError.newException("Invalid token found.\p" & err.msg, err)
    raise err0

  except NimyActionError as err:
    var err0 = XfrpSyntaxError.newException("Unexpected token is passed.\p" & err.msg, err)
    raise err0

  except NimyGotoError as err:
    var err0 = XfrpSyntaxError.newException("Goto error.\p" & err.msg, err)
    raise err0


when isMainModule:
  import os, json, std/jsonutils
  import lexer

  if paramCount() < 1:
    echo "Usage: parser [filename]"
    quit QuitFailure

  try:
    var l = buildLexerFromFilename(paramStr(1))
    let ast = parse(l)
    echo pretty(ast.toJson())

  except XfrpTypeError as err:
    echo "[Type Error] ", err.msg
    for info in err.causes:
      stderr.writeLine pretty(info)

  except XfrpSyntaxError as err:
    echo "[Syntax Error] ", err.msg
    for info in err.causes:
      stderr.writeLine pretty(info)

  except XfrpLanguageError as err:
    echo "[Language Error] ", err.msg
