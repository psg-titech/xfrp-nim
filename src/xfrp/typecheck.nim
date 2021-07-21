import tables, ropes
from strutils import join
import patty
import types, errors, codeinfos, syntax

type
  FuncType = tuple
    argTypes: seq[XfrpType]
    retType: XfrpType

  XfrpFuncTypeEnv = Table[XfrpId, FuncType]
  XfrpVarTypeEnv = Table[XfrpId, XfrpType]

  XfrpTypeEnv = object
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
    LitUnit:
      return TUnit()

    LitBool(_):
      return TBool()

    LitInt(_):
      return TInt()

    LitFloat(_):
      return TFloat()


template xfrpTypeCheck(env: XfrpTypeEnv; exp: WithCodeInfo[XfrpExpr]; ty: XfrpType): untyped =
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

    ExprBin(op, lhsRef, rhsRef):
      let (lhs, rhs) = (lhsRef[], rhsRef[])
      case op.val
      of binAdd:
        env.xfrpTypeCheck(lhs, TInt())
        env.xfrpTypeCheck(rhs, TInt())
        return TInt()

      of binEqEq:
        let lhsTy = env.xfrpTypeOf(lhs)
        env.xfrpTypeCheck(rhs, lhsTy)
        return TBool()

      of binVertVert:
        env.xfrpTypeCheck(lhs, TBool())
        env.xfrpTypeCheck(rhs, TBool())
        return TBool()

      of binLte, binLt, binRte, binRt:
        env.xfrpTypeCheck(lhs, TInt())
        env.xfrpTypeCheck(rhs, TInt())
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

when isMainModule:
  import os, options
  from sequtils import mapIt, toSeq
  import lexer, parser, envs, topsort

  proc getTopologicallySortedNodeList(env: XfrpEnv): seq[XfrpId] =
    ## Return node ID list by topologically-sorterd ordering.
    func referencesOf(n: XfrpId): seq[XfrpId] =
      env.getInnerNode(n).refNow

    let graph: ReferenceGraph[XfrpId] = (domain: toSeq(env.innerNodeIds), referencesOf: referencesOf)

    # Before sorting, check the existence of any reference cycle.
    let referenceCycle = graph.getAnyReferenceCycle()
    if referenceCycle.len > 0:
      let
        referenceCycleDiagram = (referenceCycle & referenceCycle[0]).join(" -> ")
        err = XfrpReferenceError.newException("A cycle reference is detected. At-last reference is useful for avoiding such a cycle reference. (" & referenceCycleDiagram & ")")
      for nodeId in referenceCycle:
        err.causedBy(env.getInnerNode(nodeId).id)
      raise err

    # Sorting main
    result = graph.topologicallySorted(toSeq(env.inputNodeIds))

  proc getTopologicallySortedFuncList(env: XfrpEnv): seq[XfrpId] =
    ## Return function ID list by topologically-sorterd ordering.
    func referencesOf(n: XfrpId): seq[XfrpId] =
      env.getFunction(n).deps

    let graph: ReferenceGraph[XfrpId] = (domain: toSeq(env.functionIds), referencesOf: referencesOf)

    # Before sorting, check the existence of any reference cycle.
    let referenceCycle = graph.getAnyReferenceCycle()
    if referenceCycle.len > 0:
      let
        referenceCycleDiagram = (referenceCycle & referenceCycle[0]).join(" -> ")
        err = XfrpReferenceError.newException("Definition of any recursive functions is prohibited. (" & referenceCycleDiagram & ")")
      for funcId in referenceCycle:
        err.causedBy(env.getFunction(funcId).id)
      raise err

    # Sorting main
    result = graph.topologicallySorted()

  if paramCount() < 1:
    stderr.writeLine "usage: nodeprops [filename]"
    quit QuitFailure

  var l = buildLexerFromFilename(paramStr(1))

  try:
    let
      ast = parse(l)
      env = makeEnvironmentFromModule(ast.val)
      sortedNodes = env.getTopologicallySortedNodeList()
      sortedFuncs = env.getTopologicallySortedFuncList()

    var typeEnv = initXfrpTypeEnv()

    # store function types into type environment
    for funcId in sortedFuncs:
      let f = env.getFunction(funcId)
      var argsTypeEnv = typeEnv
      for argAst in f.args:
        argsTypeEnv.addVar(argAst.val.id.val, argAst.val.ty.val)

      let exprTy = argsTypeEnv.xfrpTypeOf(f.body)
      if f.retType.val != exprTy:
        let err = XfrpTypeError.newException("Type " & $f.retType.val & " expected, but the expression has type " & $exprTy & ".")
        err.causedBy(f.body)
        raise err

      typeEnv.addFunc(f.id.val, f.args.mapIt(it.val.ty.val), f.retType.val)

    # store input node types into type environment
    for inputNodeId in env.inputNodeIds:
      let n = env.getInputNode(inputNodeId)

      # check initial value is the same type as type annotation
      n.init.map do (initExprAst: WithCodeInfo[XfrpExpr]):
        typeEnv.xfrpTypeCheck(initExprAst, n.ty.val)

      typeEnv.addVar(n.id.val, n.ty.val)

    # store inner node types into type environment
    # firstly, check types of each nodes with initial value
    # for at-last references
    for nodeId in sortedNodes:
      let n = env.getInnerNode(nodeId)

      n.init.map do (initExprAst: WithCodeInfo[XfrpExpr]):
        let initTy = typeEnv.xfrpTypeOf(initExprAst)

        n.typeOpt.map do (explicitType: WithCodeInfo[XfrpType]):
          if explicitType.val != initTy:
            let err = XfrpTypeError.newException("Type " & $explicitType.val & " expected, but the initial value has type " & $initTy & ".")
            err.causedBy(initExprAst)
            raise err

        typeEnv.addVar(n.id.val, initTy)

    # then check types of all nodes by topologically-sorting ordering
    for nodeId in sortedNodes:
      let
        n = env.getInnerNode(nodeId)
        updateTy = typeEnv.xfrpTypeOf(n.update)

      n.typeOpt.map do (explicitType: WithCodeInfo[XfrpType]):
        if explicitType.val != updateTy:
          let err = XfrpTypeError.newException("Type " & $explicitType.val & " expected, but the expression has type " & $updateTy & ".")
          err.causedBy(n.update)
          raise err

      if typeEnv.hasVarOrAdd(n.id.val, updateTy):
        let initTy = typeEnv.venv[n.id.val]
        if updateTy != initTy:
          let err = XfrpTypeError.newException("The expression has type " & $updateTy & ", but the initial value has type " & $initTy & ".")
          err.causedBy(n.update, n.init.unsafeGet())
          raise err

    # finally, check types of output nodes
    for outputNodeAst in env.outputNodeIdAndTypeOptAsts:
      match outputNodeAst.val:
        IdWithExplicitType(idAst, tyAst):
          let actualType = typeEnv.getVarType(idAst.val)
          if actualType != tyAst.val:
            let err = XfrpTypeError.newException("Output node '" & idAst.val & "' expects type " & $tyAst.val & " as its own type, but actually the type is " & $actualType & ".")
            err.causedBy(tyAst, env.getInnerNode(idAst.val).update)
            raise err

        IdWithoutAnyTypeAnnot(_):
          discard

    echo typeEnv

  except XfrpLanguageError as err:
    stderr.writeLine "[", err.name, "] ", err.msg
    for info in err.causes:
      stderr.writeLine pretty(info)
