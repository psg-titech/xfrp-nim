## (Binary) operator precedence controller and environments.

import tables
from sequtils import mapIt
import patty
import syntax, codeinfos, errors

type
  XfrpInfixAssociativity* = enum
    assocLeft, assocRight, assocNone

  XfrpInfixPrecedence = tuple
    level: Natural
    assoc: XfrpInfixAssociativity

  XfrpOpEnv* = distinct TableRef[XfrpBinOp, XfrpInfixPrecedence]


func opPrecedence*(opEnv: XfrpOpEnv): TableRef[XfrpBinOp, XfrpInfixPrecedence] =
  TableRef[XfrpBinOp, XfrpInfixPrecedence](opEnv)


func `<`(x, y: XfrpInfixPrecedence): bool =
  (x.level < y.level) or (x.level == y.level and x.assoc == assocRight)


func `>`(x, y: XfrpInfixPrecedence): bool =
  (x.level > y.level) or (x.level == y.level and x.assoc == assocLeft)


proc `~`[T](x: T): ref T =
  new result
  result[] = x


proc reparseBinaryExpressionAsExprBin(opEnv: XfrpOpEnv; exp: WithCodeInfo[XfrpExpr]; prevPrecedence = (Natural(0), assocRight)): WithCodeInfo[XfrpExpr] =
  assert(exp.val.kind == XfrpExprKind.ExprBin)

  var
    opAsts = exp.val.binOps
    termAsts = exp.val.binTerms

  while opAsts.len > 1:
    let
      opAst = opAsts[0]
      opPrecedence = opEnv.opPrecedence[opAst.val]

    if prevPrecedence > opPrecedence:
      break

    let
      nextOpAst = opAsts[1]
      nextOpPrecedence = opEnv.opPrecedence[nextOpAst.val]

    if opPrecedence > nextOpPrecedence:
      opAsts = opAsts[1..^1]
      termAsts = (ExprBin(@[opAst], termAsts[0..1]) from termAsts[0]..termAsts[1]) & termAsts[2..^1]

    elif opPrecedence < nextOpPrecedence:
      if opAsts.len == 2:
        opAsts = @[opAst]
        termAsts = @[termAsts[0], ExprBin(@[nextOpAst], termAsts[1..2]) from termAsts[1]..termAsts[2]]

        break

      else:
        let
          partialExp = ExprBin(opAsts[1..^1], termAsts[1..^1]) from termAsts[1]..termAsts[^1]
          partiallyReparsedExp = opEnv.reparseBinaryExpressionAsExprBin(partialExp, opPrecedence)

        opAsts = opAsts[0] & partiallyReparsedExp.val.binOps
        termAsts = termAsts[0] & partiallyReparsedExp.val.binTerms

    else:
      let err = XfrpSyntaxError.newException("Order of evaluation of a binary expression is ambiguous.")
      err.causedBy(opAst, nextOpAst)
      raise err

  return ExprBin(opAsts, termAsts) from exp


proc reparseBinaryExpression*(opEnv: XfrpOpEnv; exp: WithCodeInfo[XfrpExpr]): WithCodeInfo[XfrpExpr] =
  match exp.val:
    ExprBin(_, _):
      let
        exprBinReparsed = opEnv.reparseBinaryExpressionAsExprBin(exp)
        termsReparsed = exprBinReparsed.val.binTerms.mapIt(opEnv.reparseBinaryExpression(it))

      assert(exprBinReparsed.val.binOps.len == 1 and termsReparsed.len == 2)

      # let singleBinOpAst = exprBinReparsed.val.binOps[0]

      # return ExprApp($singleBinOpAst.val from singleBinOpAst, termsReparsed) from termsReparsed[0]..termsReparsed[1]

      return ExprBin(exprBinReparsed.val.binOps, termsReparsed) from exp

    ExprIf(ifAst, thenAst, elseAst):
      let
        ifAstReparsed = opEnv.reparseBinaryExpression(ifAst[])
        thenAstReparsed = opEnv.reparseBinaryExpression(thenAst[])
        elseAstReparsed = opEnv.reparseBinaryExpression(elseAst[])

      return ExprIf(~ifAstReparsed, ~thenAstReparsed, ~elseAstReparsed) from exp

    ExprApp(idAst, argAsts):
      let argsReparsed = argAsts.mapIt(opEnv.reparseBinaryExpression(it))

      return ExprApp(idAst, argsReparsed) from exp

    _:
      return exp


proc makeOperatorEnvironmentFromModule*(ast: XfrpModule): XfrpOpEnv =
  var opPrecedence = newTable[XfrpBinOp, XfrpInfixPrecedence]()

  opPrecedence[binAdd] = (Natural(80), assocLeft)
  opPrecedence[binEqEq] = (Natural(50), assocLeft)
  opPrecedence[binVertVert] = (Natural(30), assocLeft)
  opPrecedence[binLte] = (Natural(50), assocLeft)
  opPrecedence[binLt] = (Natural(50), assocLeft)
  opPrecedence[binGte] = (Natural(50), assocLeft)
  opPrecedence[binGt] = (Natural(50), assocLeft)

  result = XfrpOpEnv(opPrecedence)


when isMainModule:
  import os, json, std/jsonutils
  import lexer, parser, envs

  if paramCount() < 1:
    echo "Usage: operators [filename]"
    quit QuitFailure

  try:
    var l = buildLexerFromFilename(paramStr(1))
    let
      ast = parse(l)
      opEnv = makeOperatorEnvironmentFromModule(ast.val)
      env = makeEnvironmentFromModule(ast.val).mapForExpr do (expAst: WithCodeInfo[XfrpExpr]) -> WithCodeInfo[XfrpExpr]:
        opEnv.reparseBinaryExpression(expAst)

    echo pretty(env.toJson())

  except XfrpLanguageError as err:
    stderr.writeLine "[", err.name, "] ", err.msg
    for info in err.causes:
      stderr.writeLine pretty(info)
