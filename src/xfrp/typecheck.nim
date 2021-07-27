## XFRP type checking and type environments.

import tables, ropes, options
from sequtils import mapIt
from strutils import join
import patty
import types, errors, codeinfos, syntax, envs

type
  FuncType* = tuple
    argTypes: seq[XfrpType]
    retType: XfrpType

  XfrpFuncTypeEnv = Table[XfrpId, FuncType]
  XfrpVarTypeEnv = Table[XfrpId, XfrpType]

  XfrpTypeEnv* = object
    fenv: XfrpFuncTypeEnv
    venv: XfrpVarTypeEnv


proc initXfrpTypeEnv*: XfrpTypeEnv =
  result.fenv = initTable[XfrpId, FuncType]()
  result.venv = initTable[XfrpId, XfrpType]()


proc addFunc*(typeEnv: var XfrpTypeEnv; id: XfrpId; argTypes: seq[XfrpType]; retType: XfrpType) =
  typeEnv.fenv[id] = (argTypes, retType)


proc addVar*(typeEnv: var XfrpTypeEnv; id: XfrpId; ty: XfrpType) =
  typeEnv.venv[id] = ty


proc hasVarOrAdd*(typeEnv: var XfrpTypeEnv; id: XfrpId; ty: XfrpType): bool =
  typeEnv.venv.hasKeyOrPut(id, ty)


proc getFuncType*(typeEnv: XfrpTypeEnv; id: XfrpId): FuncType =
  typeEnv.fenv[id]


proc getVarType*(typeEnv: XfrpTypeEnv; id: XfrpId): XfrpType =
  typeEnv.venv[id]


proc `$`*(typeEnv: XfrpTypeEnv): string =
  var r: Rope
  for (funcId, funcType) in pairs(typeEnv.fenv):
    r.add funcId & " : (" & funcType.argTypes.join(", ") & ") -> " & $funcType.retType & "\p"

  for (varId, varType) in pairs(typeEnv.venv):
    r.add varId & " : " & $varType & "\p"

  result = $r


proc xfrpTypeOf*(env: XfrpTypeEnv; lit: WithCodeInfo[XfrpLiteral]): XfrpType =
  match lit.val:
    LitBool(_):
      return TBool()

    LitInt(_):
      return TInt()

    LitFloat(_):
      return TFloat()


template xfrpTypeCheck(env: XfrpTypeEnv; exp: WithCodeInfo[XfrpExpr]; ty: XfrpType) =
  let expTy = env.xfrpTypeOf(exp)
  if expTy != ty:
    let err = XfrpTypeError.newException("Expected type " & $ty & ", but got " & $expTy & ".")
    err.causedBy(exp)
    raise err


proc xfrpTypeOf*(env: XfrpTypeEnv; exp: WithCodeInfo[XfrpExpr]): XfrpType =
  match exp.val:
    ExprLiteral(lit):
      return env.xfrpTypeOf(lit)

    ExprId(id):
      if id.val notin env.venv:
        let err = XfrpTypeError.newException("Variable '" & id.val & "' has not been typed.")
        err.causedBy(exp)
        raise err

      return env.venv[id.val]

    ExprAnnot(id, annot):
      match annot.val:
        AnnotAtLast:
          if id.val notin env.venv:
            let err = XfrpTypeError.newException("Variable '" & id.val & "' has not been typed.")
            err.causedBy(id)
            raise err

          return env.venv[id.val]

    ExprBin(ops, terms):
      assert(ops.len == 1 and terms.len == 2)

      case ops[0].val
      of binAdd:
        env.xfrpTypeCheck(terms[0], TInt())
        env.xfrpTypeCheck(terms[1], TInt())
        return TInt()

      of binEqEq:
        let lhsTy = env.xfrpTypeOf(terms[0])
        env.xfrpTypeCheck(terms[1], lhsTy)
        return TBool()

      of binVertVert:
        env.xfrpTypeCheck(terms[0], TBool())
        env.xfrpTypeCheck(terms[1], TBool())
        return TBool()

      of binLte, binLt, binGte, binGt:
        env.xfrpTypeCheck(terms[0], TInt())
        env.xfrpTypeCheck(terms[1], TInt())
        return TBool()

    ExprIf(ifExprRef, thenExprRef, elseExprRef):
      let (ifExpr, thenExpr, elseExpr) = (ifExprRef[], thenExprRef[], elseExprRef[])
      env.xfrpTypeCheck(ifExpr, TBool())
      let thenExprTy = env.xfrpTypeOf(thenExpr)
      env.xfrpTypeCheck(elseExpr, thenExprTy)

      return thenExprTy

    ExprApp(id, args):
      let funTy = env.fenv[id.val]
      if funTy.argTypes.len != args.len:
        let err = XfrpTypeError.newException("Number of arguments is incorrect. Function '" & id.val & "' is " & $funTy.argTypes.len & "-arity.")
        err.causedBy(exp)
        raise err

      for i in 0..<args.len:
        env.xfrpTypeCheck(args[i], funTy.argTypes[i])

      return funTy.retType


proc makeTypeEnvironmentFromEnvironment*(env: XfrpEnv): XfrpTypeEnv =
  result = initXfrpTypeEnv()

  # store function types into type environment
  for funcId in env.functionIds:
    let f = env.getFunction(funcId)
    var argsTypeEnv = result
    for argAst in f.args:
      argsTypeEnv.addVar(argAst.val.id.val, argAst.val.ty.val)

    let exprTy = argsTypeEnv.xfrpTypeOf(f.body)
    if f.retType.val != exprTy:
      let err = XfrpTypeError.newException("Type " & $f.retType.val & " expected, but the expression has type " & $exprTy & ".")
      err.causedBy(f.body)
      raise err

    result.addFunc(f.id.val, f.args.mapIt(it.val.ty.val), f.retType.val)

  # store input node types into type environment
  for inputNodeId in env.inputNodeIds:
    let n = env.getInputNode(inputNodeId)

    # check initial value is the same type as type annotation
    if n.init.isSome:
      let initExprAst = unsafeGet(n.init)
      result.xfrpTypeCheck(initExprAst, n.ty.val)

    result.addVar(n.id.val, n.ty.val)

  # store inner node types into type environment
  # firstly, check types of each nodes with initial value
  # for at-last references
  for nodeId in env.innerNodeIds:
    let n = env.getInnerNode(nodeId)

    if n.init.isSome:
      let initExprAst = unsafeGet(n.init)
      let initTy = result.xfrpTypeOf(initExprAst)

      n.typeOpt.map do (explicitType: WithCodeInfo[XfrpType]):
        if explicitType.val != initTy:
          let err = XfrpTypeError.newException("Type " & $explicitType.val & " expected, but the initial value has type " & $initTy & ".")
          err.causedBy(initExprAst)
          raise err

      result.addVar(n.id.val, initTy)

  # then check types of all nodes by topologically-sorting ordering
  for nodeId in env.innerNodeIds:
    let
      n = env.getInnerNode(nodeId)
      updateTy = result.xfrpTypeOf(n.update)

    if n.typeOpt.isSome:
      let explicitType = unsafeGet(n.typeOpt)
      if explicitType.val != updateTy:
        let err = XfrpTypeError.newException("Type " & $explicitType.val & " expected, but the expression has type " & $updateTy & ".")
        err.causedBy(n.update)
        raise err

    if result.hasVarOrAdd(n.id.val, updateTy):
      let initTy = result.venv[n.id.val]
      if updateTy != initTy:
        let err = XfrpTypeError.newException("The expression has type " & $updateTy & ", but the initial value has type " & $initTy & ".")
        err.causedBy(n.update, n.init.unsafeGet())
        raise err

  # finally, check types of output nodes
  for outputNodeAst in env.outputNodeIdAndTypeOptAsts:
    match outputNodeAst.val:
      IdWithExplicitType(idAst, tyAst):
        let actualType = result.getVarType(idAst.val)
        if actualType != tyAst.val:
          let err = XfrpTypeError.newException("Output node '" & idAst.val & "' expects type " & $tyAst.val & " as its own type, but actually the type is " & $actualType & ".")
          err.causedBy(tyAst, env.getInnerNode(idAst.val).update)
          raise err

      IdWithoutAnyTypeAnnot(_):
        discard

when isMainModule:
  import os
  import lexer, parser, envs, operators

  if paramCount() < 1:
    stderr.writeLine "usage: nodeprops [filename]"
    quit QuitFailure

  var l = buildLexerFromFilename(paramStr(1))

  try:
    let
      ast = parse(l)
      opEnv = makeOperatorEnvironmentFromModule(ast.val)
      env = makeEnvironmentFromModule(ast.val).mapForExpr do (expAst: WithCodeInfo[XfrpExpr]) -> WithCodeInfo[XfrpExpr]:
        opEnv.reparseBinaryExpression(expAst)
      typeEnv = makeTypeEnvironmentFromEnvironment(env)

    echo typeEnv

  except XfrpLanguageError as err:
    stderr.writeLine "[", err.name, "] ", err.msg
    for info in err.causes:
      stderr.writeLine pretty(info)
