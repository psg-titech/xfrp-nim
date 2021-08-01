## (Binary) operator precedence controller and environments.

import tables
from sequtils import mapIt, anyIt
import patty
import ".."/[syntax, types, codeinfos, errors, materials]

type
  XfrpInfixPrecedence = tuple
    level: XfrpOperatorPrecedenceLevel
    assoc: XfrpOperatorAssociativity

  XfrpOpBody = object
    retType: WithCodeInfo[XfrpType]
    body: WithCodeInfo[XfrpExpr]

  XfrpOpDescription = object
    symbol: XfrpOperator
    precedence: XfrpInfixPrecedence
    body: TableRef[seq[XfrpType], XfrpOpBody]

  XfrpOpId = tuple
    moduleId: XfrpModuleId
    op: XfrpOperator

  XfrpOpEnv* = distinct TableRef[XfrpOpId, XfrpOpDescription]


func `$`(opId: XfrpOpId): string =
  opId.moduleId & "." & opId.op


proc getPrecedence(opEnv: XfrpOpEnv; opAst: WithCodeInfo[XfrpOperator]; definedIn: XfrpModuleId; materialTbl: XfrpMaterials): XfrpInfixPrecedence =
  let tbl = TableRef[XfrpOpId, XfrpOpDescription](opEnv)

  for moduleId in materialTbl.materialsOf(definedIn):
    let opId = (moduleId, opAst.val)

    if opId in tbl:
      return tbl[opId].precedence

  let err = XfrpDefinitionError.newException("Operator " & opAst.val & " is not defined.")
  err.causedBy(opAst)
  raise err



func `<`(x, y: XfrpInfixPrecedence): bool =
  (x.level < y.level) or (x.level == y.level and x.assoc == assocRight)


func `>`(x, y: XfrpInfixPrecedence): bool =
  (x.level > y.level) or (x.level == y.level and x.assoc == assocLeft)


proc `~`[T](x: T): ref T =
  new result
  result[] = x


proc reparseBinaryExpressionAsExprBin(opEnv: XfrpOpEnv; exp: WithCodeInfo[XfrpExpr]; definedIn: XfrpModuleId; materialTbl: XfrpMaterials; prevPrecedence = (Natural(0), assocRight)): WithCodeInfo[XfrpExpr] =
  assert(exp.val.kind == XfrpExprKind.ExprBin)

  var
    opAsts = exp.val.binOps
    termAsts = exp.val.binTerms

  while opAsts.len > 1:
    let
      opAst = opAsts[0]
      opPrecedence = opEnv.getPrecedence(opAst, definedIn, materialTbl)

    if prevPrecedence > opPrecedence:
      break

    let
      nextOpAst = opAsts[1]
      nextOpPrecedence = opEnv.getPrecedence(nextOpAst, definedIn, materialTbl)

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
          partiallyReparsedExp = opEnv.reparseBinaryExpressionAsExprBin(partialExp, definedIn, materialTbl, opPrecedence)

        opAsts = opAsts[0] & partiallyReparsedExp.val.binOps
        termAsts = termAsts[0] & partiallyReparsedExp.val.binTerms

    else:
      let err = XfrpSyntaxError.newException("Order of evaluation of a binary expression is ambiguous.")
      err.causedBy(opAst, nextOpAst)
      raise err

  return ExprBin(opAsts, termAsts) from exp


proc reparseBinaryExpression*(opEnv: XfrpOpEnv; exp: WithCodeInfo[XfrpExpr]; definedIn: XfrpModuleId; materialTbl: XfrpMaterials): WithCodeInfo[XfrpExpr] =
  match exp.val:
    ExprBin(_, _):
      let
        exprBinReparsed = opEnv.reparseBinaryExpressionAsExprBin(exp, definedIn, materialTbl)
        termsReparsed = exprBinReparsed.val.binTerms.mapIt(opEnv.reparseBinaryExpression(it, definedIn, materialTbl)) # recursive application to children

      assert(exprBinReparsed.val.binOps.len == 1 and termsReparsed.len == 2)

      return ExprBin(exprBinReparsed.val.binOps, termsReparsed) from exp

    ExprIf(ifAst, thenAst, elseAst):
      let
        ifAstReparsed = opEnv.reparseBinaryExpression(ifAst[], definedIn, materialTbl)
        thenAstReparsed = opEnv.reparseBinaryExpression(thenAst[], definedIn, materialTbl)
        elseAstReparsed = opEnv.reparseBinaryExpression(elseAst[], definedIn, materialTbl)

      return ExprIf(~ifAstReparsed, ~thenAstReparsed, ~elseAstReparsed) from exp

    ExprApp(idAst, argAsts):
      let argsReparsed = argAsts.mapIt(opEnv.reparseBinaryExpression(it, definedIn, materialTbl))

      return ExprApp(idAst, argsReparsed) from exp

    ExprMagic(idAndTypeAst, argAsts):
      let argsReparsed = argAsts.mapIt(opEnv.reparseBinaryExpression(it, definedIn, materialTbl))

      return ExprMagic(idAndTypeAst, argsReparsed) from exp

    _:
      return exp


proc hasOpOrFuncReference(exp: XfrpExpr): bool =
  match exp:
    ExprBin(_, _): return true
    ExprApp(_, _): return true
    ExprIf(ifAstRef, thenAstRef, elseAstRef):
      return hasOpOrFuncReference(ifAstRef[].val) or hasOpOrFuncReference(thenAstRef[].val) or hasOpOrFuncReference(elseAstRef[].val)

    ExprMagic(_, argAsts):
      return argAsts.anyIt(hasOpOrFuncReference(it.val))

    _:
      return false


proc makeOperatorEnvironmentFromModule*(ast: XfrpModule; materialTbl: XfrpMaterials): XfrpOpEnv =
  var
    precedenceTbl = initTable[XfrpOpId, XfrpInfixPrecedence]()
    descriptionTbl = newTable[XfrpOpId, XfrpOpDescription]()

  for moduleId in materialTbl.materialsOf(ast.moduleId.val):
    let module = materialTbl[moduleId]

    for defAst in module.val.defs:
      match defAst.val:
        DefInfix(opAst, level, assoc):
          if (moduleId, opAst.val) in precedenceTbl:
            let err = XfrpDefinitionError.newException("Operator precedence of an operator " & opAst.val & " has already been defined.")
            err.causedBy(defAst)
            raise err

          precedenceTbl[(moduleId, opAst.val)] = (level, assoc)

        DefOp(opAst, retTyAst, argAsts, bodyAst):
          if hasOpOrFuncReference(bodyAst.val):
            let err = XfrpReferenceError.newException("Any operators cannot have any references to operators or functions.")
            err.causedBy(bodyAst)
            raise err

          if (moduleId, opAst.val) in descriptionTbl:
            let
              oldDescription = descriptionTbl[(moduleId, opAst.val)]
              argTypes = argAsts.mapIt(it.val.ty.val)

            if argTypes in oldDescription.body:
              let err = XfrpDefinitionError.newException("Operator redefinition detected.")
              err.causedBy(opAst)
              raise err

            oldDescription.body[argTypes] = XfrpOpBody(retType: retTyAst, body: bodyAst)

          else:
            let
              body = XfrpOpBody(retType: retTyAst, body: bodyAst)
              bodyTbl = { argAsts.mapIt(it.val.ty.val): body }.newTable()

            descriptionTbl[(moduleId, opAst.val)] = XfrpOpDescription(symbol: opAst.val, body: bodyTbl)

        _: discard

  for (op, description) in mpairs(descriptionTbl):
    if op notin precedenceTbl:
      let err = XfrpDefinitionError.newException("Operator " & $op & " is defined but its precedence.")
      raise err

    description.precedence = precedenceTbl[op]

  result = XfrpOpEnv(descriptionTbl)

when isMainModule:
  import os, json, std/jsonutils
  from ".."/loaders import newXfrpLoader, load, loadMaterials

  if paramCount() < 1:
    echo "Usage: operators [filename]"
    quit QuitFailure

  try:
    let
      loader = newXfrpLoader(@[getCurrentDir()])
      ast = loader.load(paramStr(1), false)
      materialTbl = loader.loadMaterials(ast)
      opEnv = makeOperatorEnvironmentFromModule(ast.val, materialTbl)

    echo pretty(opEnv.toJson())

  except XfrpLanguageError as err:
    stderr.writeLine "[", err.name, "] ", err.msg
    for info in err.causes:
      stderr.writeLine pretty(info)
