## Code generation to C.
## Generated code is based on ISO/IEC 9899:1999 (also known as C99).

import ropes, strtabs, random, options
from strutils import indent, join
from sequtils import mapIt, zip, foldl
import patty
import ".."/[envs, typecheck, syntax, codeinfos, types]

type
  CCodeFiles* = tuple
    frpFile, frpHeaderFile, mainFile: string

  NameTables = tuple
    funcNameTable, varNameTable: StringTableRef

  CodeList = tuple
    calcCode, resultCode: string


func `?->`(calcCode, resultCode: string): CodeList =
  (calcCode, resultCode)


func `?\`(x, y: string): string =
  if x.len == 0: y else: x & "\p" & y


func `?`(ty: XfrpType): string =
  result = match ty:
    TUnit: "XFRP_UNIT"
    TBool: "XFRP_BOOL"
    TInt: "XFRP_INT"
    TFloat: "XFRP_FLOAT"
    TTuple(_): "XFRP_TUPLE" # TODO


func `?`(op: XfrpBinOp): string =
  result = case op
    of binAdd: "_op_plus"
    of binEqEq: "_op_eq_eq"
    of binVertVert: "_op_vert_vert"
    of binLte: "_op_lte"
    of binLt: "_op_lt"
    of binGte: "_op_gte"
    of binGt: "_op_gt"


const xfrpTypeDefinitionCode = """
#define XFRP_UNIT int
#define XFRP_BOOL int
#define XFRP_INT int
#define XFRP_FLOAT float
"""

const xfrpBinaryOperatorDefinitionCode = """
#define _op_plus(lhs, rhs) ((lhs) + (rhs))
#define _op_eq_eq(lhs, rhs) ((lhs) == (rhs))
#define _op_vert_vert(lhs, rhs) ((lhs) || (rhs))
#define _op_lte(lhs, rhs) ((lhs) <= (rhs))
#define _op_lt(lhs, rhs) ((lhs) < (rhs))
#define _op_gte(lhs, rhs) ((lhs) >= (rhs))
#define _op_gt(lhs, rhs) ((lhs) > (rhs))
"""


proc genFunctionNameInCode(funcId: XfrpId): string =
  result = "_func_" & funcId


proc genArgNameInCode(argId: XfrpId): string =
  result = "_arg_" & argId


proc genNodeNameInCode(nodeId: XfrpId): string =
  result = "_node_" & nodeId


proc genFreshVariableInCode: string =
  var uid {.global.} = rand(uint16)

  result = "_genvar_" & $uid
  inc uid


proc codegenExp(exp: WithCodeInfo[XfrpExpr]; typeEnv: XfrpTypeEnv; nameTbl: NameTables; extraVarTbl = newStringTable()): CodeList =
  match exp.val:
    ExprLiteral(litAst):
      match litAst.val:
        LitUnit:
          return "" ?-> ("(" & ?TUnit() & ")0")
        LitBool(val):
          return "" ?-> ("(" & ?TBool() & ")" & (if val: "1" else: "0"))
        LitInt(val):
          return "" ?-> ("(" & ?TInt() & ")" & $val)
        LitFloat(val):
          return "" ?-> ("(" & ?TFloat() & ")" & $val)

    ExprId(idAst):
      let id = idAst.val
      if id in extraVarTbl:
        return "" ?-> extraVarTbl[id]

      return "" ?-> nameTbl.varNameTable[id]

    ExprAnnot(idAst, annotAst):
      match annotAst.val:
        AnnotAtLast:
          let id = idAst.val
          if id in extraVarTbl:
            return "" ?-> (extraVarTbl[id] & "_atlast")

          return "" ?-> (nameTbl.varNameTable[id] & "_atlast")

    ExprBin(opAst, lhsAstRef, rhsAstRef):
      let
        (lhsCalc, lhsResult) = codegenExp(lhsAstRef[], typeEnv, nameTbl, extraVarTbl)
        (rhsCalc, rhsResult) = codegenExp(rhsAstRef[], typeEnv, nameTbl, extraVarTbl)

        ty = typeEnv.xfrpTypeOf(exp)
        freshVar = genFreshVariableInCode()

      return (lhsCalc ?\ rhsCalc ?\ (?ty & " " & freshVar & " = " &
        ?opAst.val & "(" & lhsResult & ", " & rhsResult & ");")) ?-> freshVar

    ExprIf(ifExprAstRef, thenExprAstRef, elseExprAstRef):
      let
        (ifCalc, ifResult) = codegenExp(ifExprAstRef[], typeEnv, nameTbl, extraVarTbl)
        (thenCalc, thenResult) = codegenExp(thenExprAstRef[], typeEnv, nameTbl, extraVarTbl)
        (elseCalc, elseResult) = codegenExp(elseExprAstRef[], typeEnv, nameTbl, extraVarTbl)

        ty = typeEnv.xfrpTypeOf(exp)
        freshVar = genFreshVariableInCode()

      return (ifCalc ?\ (?ty & " " & freshVar & ";") ?\ ("if (" & ifResult & ") {") ?\
        indent(thenCalc ?\ (freshVar & " = " & thenResult & ";"), 2) ?\ "} else {" ?\
        indent(elseCalc ?\ (freshVar & " = " & elseResult & ";"), 2) ?\ "}") ?-> freshVar

    ExprApp(appIdAst, appArgAsts):
      let
        argCodes = appArgAsts.mapIt(codegenExp(it, typeEnv, nameTbl, extraVarTbl))
        argTypes = appArgAsts.mapIt(typeEnv.xfrpTypeOf(it))

        resultTy = typeEnv.xfrpTypeOf(exp)
        resultFreshVar = genFreshVariableInCode()
      var
        simpleArgs: seq[string]
        argCalcWithResults: seq[string]

      for (argCode, argType) in zip(argCodes, argTypes):
        let (argCalc, argResult) = argCode
        if argCalc.len > 0:
          simpleArgs.add genFreshVariableInCode()
          argCalcWithResults.add argCalc ?\ (?argType & " " & simpleArgs[^1] & " = " & argResult & ";")
        else:
          simpleArgs.add argResult

      return (argCalcWithResults.foldl(a ?\ b, "") ?\
        (?resultTy & " " & resultFreshVar & " = " &
        nameTbl.funcNameTable[appIdAst.val] & "(" & simpleArgs.join(", ") & ");")) ?-> resultFreshVar


proc genFrpFile(env: XfrpEnv; typeEnv: XfrpTypeEnv; nameTbl: NameTables): string =
  var r: Rope

  r.add "#include \"Example.h\"\p\p"

  r.add xfrpTypeDefinitionCode
  r.add xfrpBinaryOperatorDefinitionCode

  r.add "/* functions */\p"
  for funcId in env.functionIds:
    let
      funcDef = env.getFunction(funcId)
      (_, retType) = typeEnv.getFuncType(funcId)

    var
      argsTbl = newStringTable(modeCaseSensitive)
      argsTypeEnv = typeEnv
    for arg in funcDef.args:
      argsTbl[arg.val.id.val] = genArgNameInCode(arg.val.id.val)
      argsTypeEnv.addVar(arg.val.id.val, arg.val.ty.val)

    let (bodyCalc, bodyResult) = codegenExp(funcDef.body, argsTypeEnv, nameTbl, argsTbl)

    r.add ?retType & " " & nameTbl.funcNameTable[funcId] & "("
    r.add funcDef.args.mapIt(?it.val.ty.val & " " & argsTbl[it.val.id.val]).join(", ")
    r.add ") {\p"
    if bodyCalc.len > 0: r.add indent(bodyCalc, 2) & "\p"
    r.add "  return " & bodyResult & ";\p}\p\p"

  r.add "/* node initializers */\p"
  for nodeId in env.inputNodeIds:
    let inputDef = env.getInputNode(nodeId)

    if inputDef.init.isNone: continue

    let
      ty = typeEnv.getVarType(inputDef.id.val)
      initExprAst = inputDef.init.unsafeGet()
      (initCalc, initResult) = codegenExp(initExprAst, typeEnv, nameTbl)

    r.add "void _init" & nameTbl.varNameTable[nodeId] & "(" & ?ty & " *output) {\p"
    if initCalc.len > 0: r.add indent(initCalc, 2) & "\p"
    r.add "  *output = " & initResult & ";\p}\p\p"

  for nodeId in env.innerNodeIds:
    let nodeDef = env.getInnerNode(nodeId)

    if nodeDef.init.isNone: continue

    let
      ty = typeEnv.getVarType(nodeDef.id.val)
      initExprAst = nodeDef.init.unsafeGet()
      (initCalc, initResult) = codegenExp(initExprAst, typeEnv, nameTbl)

    r.add "void _init" & nameTbl.varNameTable[nodeId] & "(" & ?ty & " *output) {\p"
    if initCalc.len > 0: r.add indent(initCalc, 2) & "\p"
    r.add "  *output = " & initResult & ";\p}\p\p"

  r.add "/* node updater */\p"
  for nodeId in env.innerNodeIds:
    let
      nodeDef = env.getInnerNode(nodeId)
      ty = typeEnv.getVarType(nodeId)
      updateExprAst = nodeDef.update
      (updateCalc, updateResult) = codegenExp(updateExprAst, typeEnv, nameTbl)

    r.add "void _update" & nameTbl.varNameTable[nodeId] & "("
    for refNowNodeId in nodeDef.refNow:
      let refNodeTy = typeEnv.getVarType(refNowNodeId)
      r.add ?refNodeTy & " " & nameTbl.varNameTable[refNowNodeId] & ", "

    for refAtLastNodeId in nodeDef.refAtLast:
      let refNodeTy = typeEnv.getVarType(refAtLastNodeId)
      r.add ?refNodeTy & " " & nameTbl.varNameTable[refAtLastNodeId] & "_atlast, "

    r.add ?ty & " *output) {\p"
    if updateCalc.len > 0: r.add indent(updateCalc, 2) & "\p"
    r.add "  *output = " & updateResult & ";\p}\p\p"

  r.add "void ActivateExample(void) {\p"
  r.add "  int turn = 0;\p"
  r.add "XFRP_LOOP_BEGIN:\p"
  r.add "  /* Get input values */\p"
  r.add "  Input();\p\p"
  r.add "  /* Update nodes by topologically-sorted ordering */\p"
  r.add "  /* Output values */\p"
  r.add "  Output();\p\p"
  r.add "  /* Prepare for the next iteration */\p"
  r.add "  turn ^= 1;\p\p"
  r.add "  goto XFRP_LOOP_BEGIN;\p"
  r.add "}\p"

  result = $r


proc genHeaderFile: string =
  var r: Rope

  r.add "#ifndef EXAMPLE_H\p"
  r.add "#define EXAMPLE_H\p\p"

  r.add "extern void Input(void);\p"
  r.add "extern void Output(void);\p\p"

  r.add "extern void ActivateExample(void);\p"

  r.add "\p#endif\p"

  result = $r


proc genMainFile: string =
  var r: Rope

  r.add "/* Generated by XFRP compiler */\p"

  r.add "#include \"Example.h\"\p\p"

  r.add "void Input(void) {\p  /* Your code goes here ... */\p}\p\p"
  r.add "void Output(void) {\p  /* Your code goes here ... */\p}\p\p"

  r.add "int main(int argc, char *argv[]) {\p"
  r.add "  ActivateExample();\p"
  r.add "  return 0;\p}\p"

  result = $r


proc codegen*(env: XfrpEnv; typeEnv: XfrpTypeEnv): CCodeFiles =
  let
    funcNameTable = newStringTable(modeCaseSensitive)
    varNameTable = newStringTable(modeCaseSensitive)

  randomize()

  for funcId in env.functionIds:
    funcNameTable[funcId] = genFunctionNameInCode(funcId)

  for nodeId in env.nodeIds:
    varNameTable[nodeId] = genNodeNameInCode(nodeId)

  result.frpFile = genFrpFile(env, typeEnv, (funcNameTable, varNameTable))
  result.frpHeaderFile = genHeaderFile()
  result.mainFile = genMainFile()
