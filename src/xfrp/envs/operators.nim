## (Binary) operator precedence controller and environments.

import tables
from sequtils import mapIt
import patty
import ".."/[syntax, codeinfos, errors]
from ".."/loaders import XfrpMaterials

type
  XfrpInfixPrecedence = tuple
    level: XfrpOperatorPrecedenceLevel
    assoc: XfrpOperatorAssociativity

  XfrpOpEnv* = distinct TableRef[XfrpOperator, XfrpInfixPrecedence]


proc getPrecedence(opEnv: XfrpOpEnv; opAst: WithCodeInfo[XfrpOperator]): XfrpInfixPrecedence =
  let tbl = TableRef[XfrpOperator, XfrpInfixPrecedence](opEnv)

  if opAst.val notin tbl:
    let err = XfrpDefinitionError.newException("Operator " & opAst.val & " has no precedence. Use infixl, infixr or infix for setting operator precedences.")
    err.causedBy(opAst)
    raise err

  result = tbl[opAst.val]


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
      opPrecedence = opEnv.getPrecedence(opAst)

    if prevPrecedence > opPrecedence:
      break

    let
      nextOpAst = opAsts[1]
      nextOpPrecedence = opEnv.getPrecedence(nextOpAst)

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
        termsReparsed = exprBinReparsed.val.binTerms.mapIt(opEnv.reparseBinaryExpression(it)) # recursive application to children

      assert(exprBinReparsed.val.binOps.len == 1 and termsReparsed.len == 2)

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

    ExprMagic(idAndTypeAst, argAsts):
      let argsReparsed = argAsts.mapIt(opEnv.reparseBinaryExpression(it))

      return ExprMagic(idAndTypeAst, argsReparsed) from exp

    _:
      return exp


proc makeOperatorEnvironmentFromModule*(ast: XfrpModule): XfrpOpEnv =
  var opPrecedence = newTable[XfrpOperator, XfrpInfixPrecedence]()

  for defAst in ast.defs:
    match defAst.val:
      DefInfix(opAst, level, assoc):
        if opAst.val in opPrecedence:
          let err = XfrpDefinitionError.newException("Operator precedence of an operator " & opAst.val & " has already been defined.")
          err.causedBy(defAst)
          raise err

        opPrecedence[opAst.val] = (level, assoc)

      _: discard

  result = XfrpOpEnv(opPrecedence)


when isMainModule:
  import os, json, std/jsonutils
  from sequtils import toSeq
  from ".."/loaders import newXfrpLoader, load, loadMaterials

  if paramCount() < 1:
    echo "Usage: operators [filename]"
    quit QuitFailure

  try:
    let
      loader = newXfrpLoader(@[getCurrentDir()])
      ast = loader.load(paramStr(1), false)
      materials = loader.loadMaterials(ast)
      opEnv = makeOperatorEnvironmentFromModule(ast.val)

    echo pretty((entry: opEnv, materials: toSeq(values(materials)).mapIt(makeOperatorEnvironmentFromModule(it.val))).toJson())

  except XfrpLanguageError as err:
    stderr.writeLine "[", err.name, "] ", err.msg
    for info in err.causes:
      stderr.writeLine pretty(info)
