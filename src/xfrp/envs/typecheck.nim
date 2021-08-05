## XFRP type checking and type environments.

import tables, ropes, options
from sequtils import mapIt
from strutils import join
import patty
import ".."/[types, errors, codeinfos, syntax, materials]
import operators, functions, nodes

type
  XfrpFuncType* = tuple
    argTypes: seq[XfrpType]
    retType: XfrpType

  XfrpOpIdAndArgs = tuple
    id: XfrpOpId
    args: seq[XfrpType]

  XfrpFuncTypeEnv = Table[XfrpFuncId, XfrpFuncType]
  XfrpVarTypeEnv = Table[XfrpId, XfrpType]
  XfrpOpTypeEnv = Table[XfrpOpIdAndArgs, XfrpType]

  XfrpTypeEnv* = object
    fenv: XfrpFuncTypeEnv
    venv: XfrpVarTypeEnv
    openv: XfrpOpTypeEnv


proc initXfrpTypeEnv*: XfrpTypeEnv =
  result.fenv = initTable[XfrpFuncId, XfrpFuncType]()
  result.venv = initTable[XfrpId, XfrpType]()
  result.openv = initTable[XfrpOpIdAndArgs, XfrpType]()


proc addFunc*(typeEnv: var XfrpTypeEnv; id: XfrpFuncId; argTypes: seq[XfrpType]; retType: XfrpType) =
  typeEnv.fenv[id] = (argTypes, retType)


proc addVar*(typeEnv: var XfrpTypeEnv; id: XfrpId; ty: XfrpType) =
  typeEnv.venv[id] = ty


proc addVar*(typeEnv: var XfrpTypeEnv; idAndTy: XfrpIdAndType) =
  typeEnv.addVar(idAndTy.id.val, idAndTy.ty.val)


proc addOp*(typeEnv: var XfrpTypeEnv; id: XfrpOpId; argTypes: seq[XfrpType]; retType: XfrpType) =
  typeEnv.openv[(id, argTypes)] = retType


proc hasVarOrAdd*(typeEnv: var XfrpTypeEnv; id: XfrpId; ty: XfrpType): bool =
  typeEnv.venv.hasKeyOrPut(id, ty)


proc getFuncType*(typeEnv: XfrpTypeEnv; id: XfrpId; definedIn: XfrpModuleId; materialTbl: XfrpMaterials): XfrpFuncType =
  for moduleId in materialTbl.materialsOf(definedIn):
    if (moduleId, id) in typeEnv.fenv:
      return typeEnv.fenv[(moduleId, id)]

  let err = XfrpReferenceError.newException("")
  raise err


proc getFuncType*(typeEnv: XfrpTypeEnv; id: XfrpFuncId): XfrpFuncType =
  typeEnv.fenv[id]


proc getVarType*(typeEnv: XfrpTypeEnv; id: XfrpId): XfrpType =
  typeEnv.venv[id]


proc getOpType*(typeEnv: XfrpTypeEnv; op: XfrpOperator; argTypes: seq[XfrpType]; definedIn: XfrpModuleId; materialTbl: XfrpMaterials): XfrpType =
  for moduleId in materialTbl.materialsof(definedIn):
    if ((moduleId, op), argTypes) in typeEnv.openv:
      return typeEnv.openv[((moduleId, op), argTypes)]

  let err = XfrpReferenceError.newException("")
  raise err


proc `$`*(typeEnv: XfrpTypeEnv): string =
  var r: Rope
  for (funcId, funcType) in pairs(typeEnv.fenv):
    r.add $funcId & " : (" & funcType.argTypes.join(", ") & ") -> " & $funcType.retType & "\p"

  for (varId, varType) in pairs(typeEnv.venv):
    r.add $varId & " : " & $varType & "\p"

  result = $r


proc xfrpTypeOf*(env: XfrpTypeEnv; lit: WithCodeInfo[XfrpLiteral]): XfrpType =
  match lit.val:
    LitBool(_):
      return TBool()

    LitInt(_):
      return TInt()

    LitFloat(_):
      return TFloat()


template xfrpTypeCheck(env: XfrpTypeEnv; exp: WithCodeInfo[XfrpExpr]; ty: XfrpType; definedIn: XfrpModuleId; materialTbl: XfrpMaterials) =
  let expTy = env.xfrpTypeOf(exp, definedIn, materialTbl)
  if expTy != ty:
    let err = XfrpTypeError.newException("Expected type " & $ty & ", but got " & $expTy & ".")
    err.causedBy(exp)
    raise err


proc xfrpTypeOf*(env: XfrpTypeEnv; exp: WithCodeInfo[XfrpExpr]; definedIn: XfrpModuleId; materialTbl: XfrpMaterials): XfrpType =
  match exp.val:
    ExprLiteral(lit):
      return env.xfrpTypeOf(lit)

    ExprId(id):
      if id.val notin env.venv:
        let err = XfrpTypeError.newException("Variable '" & id.val & "' has not been typed.")
        err.causedBy(exp)
        raise err

      return env.getVarType(id.val)

    ExprAnnot(id, annot):
      match annot.val:
        AnnotAtLast:
          if id.val notin env.venv:
            let err = XfrpTypeError.newException("Variable '" & id.val & "' has not been typed.")
            err.causedBy(id)
            raise err

          return env.getVarType(id.val)

    ExprBin(ops, terms):
      assert(ops.len == 1 and terms.len == 2)

      let termTypes = terms.mapIt(env.xfrpTypeOf(it, definedIn, materialTbl))

      try:
        return env.getOpType(ops[0].val, termTypes, definedIn, materialTbl)

      except XfrpReferenceError as err:
        err.msg = "There are no implementations of an operator " & ops[0].val & "."
        err.causedBy(exp)
        raise err

    ExprIf(ifExprRef, thenExprRef, elseExprRef):
      let (ifExpr, thenExpr, elseExpr) = (ifExprRef[], thenExprRef[], elseExprRef[])
      env.xfrpTypeCheck(ifExpr, TBool(), definedIn, materialTbl)
      let thenExprTy = env.xfrpTypeOf(thenExpr, definedIn, materialTbl)
      env.xfrpTypeCheck(elseExpr, thenExprTy, definedIn, materialTbl)

      return thenExprTy

    ExprApp(id, args):
      let funTy = env.getFuncType(id.val, definedIn, materialTbl)
      if funTy.argTypes.len != args.len:
        let err = XfrpTypeError.newException("Number of arguments is incorrect. Function '" & id.val & "' is " & $funTy.argTypes.len & "-arity.")
        err.causedBy(exp)
        raise err

      for i in 0..<args.len:
        env.xfrpTypeCheck(args[i], funTy.argTypes[i], definedIn, materialTbl)

      return funTy.retType

    ExprMagic(idAndTyAst, args):
      for arg in args:
        discard env.xfrpTypeOf(arg, definedIn, materialTbl)

      return idAndTyAst.val.ty.val


proc makeTypeEnvironment*(materialTbl: XfrpMaterials; opEnv: XfrpOpEnv; funcEnv: XfrpFuncEnv; nodeEnv: XfrpNodeEnv): XfrpTypeEnv =
  result = initXfrpTypeEnv()
  let rootModuleId = materialTbl.getRootId()

  # store operator types into type environment
  for opId in opEnv:
    let opDesc = opEnv.getOperator(opId)
    for availableArgTy in availableArgTypes(opDesc):
      let opBody = opDesc.getbody(availableArgTy)
      var argsTypeEnv = result

      for argAst in opBody.args:
        argsTypeEnv.addVar(argAst.val)

      argsTypeEnv.xfrpTypeCheck(opBody.body, opBody.retType.val, opId.moduleId, materialTbl)
      result.addOp(opId, availableArgTy, opBody.retType.val)

  # store function types into type environment
  for funcId in funcEnv:
    let funcDesc = funcEnv.getFunction(funcId)
    var argsTypeEnv = result
    for argAst in funcDesc.args:
      argsTypeEnv.addVar(argAst.val)

    argsTypeEnv.xfrpTypeCheck(funcDesc.body, funcDesc.retType.val, funcId.module, materialTbl)
    result.addFunc(funcId, funcDesc.args.mapIt(it.val.ty.val), funcDesc.retType.val)

  # check initialize value types
  for nodeId in nodeEnv:
    let nodeDesc = nodeEnv.getNode(nodeId)

    if nodeDesc.initOpt.isSome:
      # * The code comment-out below will cause a fatal error of code generation.
      # let initExprAst = nodeDesc.initOpt.get

      if nodeDesc.isInput:
        result.xfrpTypeCheck(nodeDesc.initOpt.get, nodeDesc.inputType.val, rootModuleId, materialTbl)

        result.addVar(nodeDesc.id.val, nodeDesc.inputType.val)

      else:
        let initTy = result.xfrpTypeOf(nodeDesc.initOpt.get, rootModuleId, materialTbl)
        nodeDesc.innerTypeOpt.map do (explicitTyAst: WithCodeInfo[XfrpType]):
          if explicitTyAst.val != initTy:
            let err = XfrpTypeError.newException("Type " & $explicitTyAst.val & " expected, but the initial value has type " & $initTy & ".")
            err.causedBy(nodeDesc.initOpt.get)
            raise err

        result.addVar(nodeDesc.id.val, initTy)

    elif nodeDesc.isInput:
      result.addVar(nodeDesc.id.val, nodeDesc.inputType.val)

  # then check types of all nodes by topologically-sorting ordering
  for nodeId in nodeEnv.innerNodeIds:
    let
      nodeDesc = nodeEnv.getNode(nodeId)
      updateTy = result.xfrpTypeOf(nodeDesc.update, rootModuleId, materialTbl)

    if nodeDesc.innerTypeOpt.isSome:
      let explicitType = get(nodeDesc.innerTypeOpt)
      if explicitType.val != updateTy:
        let err = XfrpTypeError.newException("Type " & $explicitType.val & " expected, but the expression has type " & $updateTy & ".")
        err.causedBy(nodeDesc.update)
        raise err

    if result.hasVarOrAdd(nodeDesc.id.val, updateTy):
      let initTy = result.venv[nodeDesc.id.val]
      if updateTy != initTy:
        let err = XfrpTypeError.newException("The expression has type " & $updateTy & ", but the initial value has type " & $initTy & ".")
        err.causedBy(nodeDesc.update, nodeDesc.initOpt.get())
        raise err


when isMainModule:
  import os, json, std/jsonutils
  from ".."/loaders import newXfrpLoader, load, loadMaterials

  if paramCount() < 1:
    echo "Usage: nodes [filename]"
    quit QuitFailure

  try:
    let
      loader = newXfrpLoader(@[getCurrentDir()])
      ast = loader.load(paramStr(1), false)
      materials = loader.loadMaterials(ast)
      opEnv = makeOperatorEnvironmentFromModule(materials)
      funcEnv = makeFunctionEnvironment(materials, opEnv)
      nodeEnv = makeNodeEnvironment(materials, opEnv)

    for exp in exprs(nodeEnv):
      if not funcEnv.checkFuncValidity(exp, materials.getRootId(), materials):
        let err = XfrpReferenceError.newException("Undefined function is called.")
        err.causedBy(exp)
        raise err

    let typeEnv = makeTypeEnvironment(materials, opEnv, funcEnv, nodeEnv)

    echo pretty(typeEnv.toJson())

  except XfrpLanguageError as err:
    stderr.writeLine "[", err.name, "] ", err.msg
    for info in err.causes:
      stderr.writeLine pretty(info)
