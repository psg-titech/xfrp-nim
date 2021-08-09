## XFRP parser.
## The parser will produce an AST from a token sequence.
##
## **See also:**
## * `tokens <tokens.html>`_ for XFRP tokens
## * `syntax <syntax.html>`_ for XFRP ASTs
## * `nimly <https://github.com/loloicci/nimly>`_ for parser generator macro ``nimy``

import options
from strutils import parseInt, parseFloat
import nimly

import tokens, syntax, types, codeinfos, errors, compilerflags
from lexer import XfrpLexer

export lexer.XfrpLexer

func ignores(tk: XfrpToken): bool =
  tk.kind in {XfrpTokenKind.Ignore, XfrpTokenKind.Comment}


proc `~`[T](x: T): ref T =
  new result
  result[] = x


var globalCompilerFlags: set[CompilerFlag] = {}


when defined(nimdoc):
  const xfrpParser: ParsingTable[XfrpToken] = default(ParsingTable[XfrpToken])

  proc parse[T, S](parser: Parser[S]; lexer: NimlLexer[T]): WithCodeInfo[XfrpModule] =
    discard

else:
  nimy xfrpParser[XfrpToken]:
    progModule[WithCodeInfo[XfrpModule]]:
      Module Id In inputIdAndTypes Out idAndTypeOpts definitions:
        return makeXfrpModule(($2).idStr from $2, $4, $6, defs = $7) from ($1)..($7)[^1]

      Module Id In inputIdAndTypes Out idAndTypeOpts emitStmts definitions:
        return makeXfrpModule(($2).idStr from $2, $4, $6, emits = $7, defs = $8) from ($1)..($8)[^1]

      Module Id In inputIdAndTypes Out idAndTypeOpts useModules definitions:
        return makeXfrpModule(($2).idStr from $2, $4, $6, uses = $7, defs = $8) from ($1)..($8)[^1]

      Module Id In inputIdAndTypes Out idAndTypeOpts useModules emitStmts definitions:
        return makeXfrpModule(($2).idStr from $2, $4, $6, $7, $8, $9) from ($1)..($9)[^1]

      Material Id useModules[] definitions:
        if ($3).len > 0:
          return makeXfrpMaterial(($2).idStr from $2, uses = ($3)[0], defs = $4) from ($1)..($4)[^1]

        else:
          return makeXfrpMaterial(($2).idStr from $2, defs = $4) from ($1)..($4)[^1]

      Material Id useModules[] emitStmts definitions:
        if ($3).len > 0:
          return makeXfrpMaterial(($2).idStr from $2, ($3)[0], $4, $5) from ($1)..($5)[^1]

        else:
          return makeXfrpMaterial(($2).idStr from $2, emits = $4, defs = $5) from ($1)..($5)[^1]

    emitStmt[WithCodeInfo[XfrpEmit]]:
      Emit In Id TripleQuoted:
        return (($3).idStr, ($4).tqStr[3..^4]) from ($1)..($4)

    emitStmts[seq[WithCodeInfo[XfrpEmit]]]:
      emitStmt:
        return @[$1]

      emitStmt emitStmts:
        return ($1) & $2

    idAndType[WithCodeInfo[XfrpIdAndType]]:
      Id Colon typeSpecific:
        return (($1).idStr from $1, $3) from ($1)..($3)

    idAndTypes[seq[WithCodeInfo[XfrpIdAndType]]]:
      idAndType:
        return @[$1]

      idAndType Comma idAndTypes:
        return $1 & $3

    inputIdAndType[WithCodeInfo[XfrpInput]]:
      idAndType:
        return InputWithoutInit($1) from $1

      Id LParen nodeInitExpr RParen Colon typeSpecific:
        return InputWithInit((($1).idStr from $1, $6) from ($1)..($6), $3) from ($1)..($6)

    inputIdAndTypes[seq[WithCodeInfo[XfrpInput]]]:
      inputIdAndType:
        return @[$1]

      inputIdAndType Comma inputIdAndTypes:
        return $1 & $3

    idAndTypeOpt[WithCodeInfo[XfrpIdAndTypeOpt]]:
      Id Colon typeSpecific:
        return IdWithExplicitType(($1).idStr from $1, $3) from ($1)..($3)

      Id:
        return IdWithoutAnyTypeAnnot(($1).idStr from $1) from $1

    idAndTypeOpts[seq[WithCodeInfo[XfrpIdAndTypeOpt]]]:
      idAndTypeOpt:
        return @[$1]

      idAndTypeOpt Comma idAndTypeOpts:
        return $1 & $3

    typeSpecific[WithCodeInfo[XfrpType]]:
      primTypeSpecific:
        return $1

    primTypeSpecific[WithCodeInfo[XfrpType]]:
      Id:
        case ($1).idStr
        of "Bool": return TBool() from $1
        of "Int": return TInt() from $1
        of "Float": return TFloat() from $1
        else:
          let err = XfrpTypeError.newException("No such type is defined.")
          err.causedBy($1)
          raise err

    useModules[seq[WithCodeInfo[XfrpModuleId]]]:
      Use modules:
        return $2

    modules[seq[WithCodeInfo[XfrpModuleId]]]:
      moduleId:
        return @[$1]

      moduleId Comma modules:
        return ($1) & $3

    moduleId[WithCodeInfo[XfrpModuleId]]:
      Id:
        return ($1).idStr from $1

      Id Slash moduleId:
        return (($1).idStr & "/" & ($3).val) from ($1)..($3)

    definitions[seq[WithCodeInfo[XfrpDefinition]]]:
      definition:
        return @[$1]

      definition definitions:
        return $1 & $2

    definition[WithCodeInfo[XfrpDefinition]]:
      Node idAndTypeOpt Equal expression:
        return DefNode($2, none(WithCodeInfo[XfrpExpr]), $4) from ($1)..($4)

      Node nodeInitDef idAndTypeOpt Equal expression:
        return DefNode($3, some($2), $5) from ($1)..($5)

      Function Id LParen idAndTypes RParen Colon typeSpecific Equal expression:
        return DefFunc(($2).idStr from $2, $7, $4, $9) from ($1)..($9)

      Function operator LParen idAndTypes RParen Colon typeSpecific Equal expression:
        return DefOp($2, $7, $4, $9) from ($1)..($9)

      InfixLeft Digits operator:
        return DefInfix($3, Natural(($2).intStr.parseInt()), assocLeft) from ($1)..($3)

      InfixRight Digits operator:
        return DefInfix($3, Natural(($2).intStr.parseInt()), assocRight) from ($1)..($3)

      InfixNone Digits operator:
        return DefInfix($3, Natural(($2).intStr.parseInt()), assocNone) from ($1)..($3)

      Init Id Equal expression:
        if flagAutoInitExt in globalCompilerFlags:
          return DefInit(($2).idStr from $2, $4) from ($1)..($4)

        else:
          let err = XfrpSyntaxError.newException("Initialize expression definition is not supported. Use -x=autoinit option.")
          err.causedBy(($1)..($4))
          raise err

    expression[WithCodeInfo[XfrpExpr]]:
      binaryExpression:
        return $1

      If expression Then expression Else expression:
        return ExprIf(~($2), ~($4), ~($6)) from ($1)..($6)

    binaryExpression[WithCodeInfo[XfrpExpr]]:
      primitiveExpression:
        return $1

      primitiveExpression operator binaryExpression:
        if ($3).val.kind == XfrpExprKind.ExprBin:
          return ExprBin(($2) & ($3).val.binOps, ($1) & ($3).val.binTerms) from ($1)..($3)
        else:
          return ExprBin(@[$2], @[$1, $3]) from ($1)..($3)

    primitiveExpression[WithCodeInfo[XfrpExpr]]:
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

    annotation[WithCodeInfo[XfrpAnnotation]]:
      Last:
        return AnnotAtLast() from $1

    appArguments[seq[WithCodeInfo[XfrpExpr]]]:
      expression:
        return @[$1]

      expression Comma appArguments:
        return $1 & $3

    nodeInitDef[WithCodeInfo[XfrpExpr]]:
      Init LBracket nodeInitExpr RBracket:
        return ($3).val from ($1)..($4)

    nodeInitExpr[WithCodeInfo[XfrpExpr]]:
      literal:
        return ExprLiteral($1) from $1

    operator[WithCodeInfo[XfrpOperator]]:
      Operator:
        return ($1).opStr from $1

      Slash:
        return "/" from $1

    literal[WithCodeInfo[XfrpLiteral]]:
      True:
        return LitBool(true) from $1
      False:
        return LitBool(false) from $1
      Digits:
        return LitInt(($1).intStr.parseInt()) from $1
      FDigits:
        return LitFloat(($1).floatStr.parseFloat()) from $1


proc parse*(l: var XfrpLexer; flags: set[CompilerFlag] = {}): WithCodeInfo[XfrpModule] =
  ## Parse a token sequence with given lexer.
  globalCompilerFlags = flags

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
    var err0 = XfrpSyntaxError.newException(err.msg, err)
    raise err0

  except NimyGotoError as err:
    var err0 = XfrpSyntaxError.newException("Goto error.\p" & err.msg, err)
    raise err0
