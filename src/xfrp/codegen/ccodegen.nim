## Code generation to C.
## Generated code is based on ISO/IEC 9899:1999 (also known as C99).

import ropes, strtabs, random, options
from strutils import indent, join, toUpperAscii, multiReplace
from sequtils import mapIt, foldl, toSeq, unzip
import patty
import ".."/[envs, syntax, codeinfos, types]

type
  CCodeFiles* = tuple
    frpFile, frpHeaderFile, mainFile: string

  NameTables = tuple
    opNameTable, funcNameTable, varNameTable: StringTableRef

  CodeList = tuple
    calcCode, resultCode: string


const xfrpTypeDefinitionCode = """
#define XFRP_UNIT int
#define XFRP_BOOL _Bool
#define XFRP_INT int
#define XFRP_FLOAT float
"""


func xfrpEscape(s: string): string =
  result = s.multiReplace({
    "!": "bang_",
    "#": "hash_",
    "$": "dollar_",
    "%": "percent_",
    "&": "and_",
    "*": "star_",
    "+": "plus_",
    ".": "dot_",
    "/": "slash_",
    "<": "lt_",
    "=": "eq_",
    ">": "gt_",
    "?": "question_",
    "@": "at_",
    "\\": "backslash_",
    "^": "caret_",
    "|": "vert_",
    "-": "minus_",
    "~": "tilde_"})


func `?->`(calcCode, resultCode: string): CodeList =
  (calcCode, resultCode)


func `?\`(x, y: string): string =
  if x.len == 0: y else: x & "\p" & y


func `?`(ty: XfrpType): string =
  result = match ty:
    TBool: "XFRP_BOOL"
    TInt: "XFRP_INT"
    TFloat: "XFRP_FLOAT"


func `>?<`(opName: string; types: seq[XfrpType]): string =
  result = opName & "_" & types.mapIt(short(it)).join()


proc opKey(opId: XfrpOpId): string =
  result = opId.moduleId & '.' & opId.op


proc funcKey(funcId: XfrpFuncId): string =
  result = funcId.module & '.' & funcId.id


# mangling procedures

proc genOperatorNameInCode(opId: string): string =
  var uid {.global.} = rand(uint16)

  result = "op_" & xfrpEscape(opId) & $uid
  inc uid


proc genFunctionNameInCode(funcId: XfrpId): string =
  var uid {.global.} = rand(uint16)

  result = "func_" & funcId & "_" & $uid
  inc uid


proc genArgNameInCode(argId: XfrpId): string =
  result = "arg_" & argId


proc genNodeNameInCode(nodeId: XfrpId): string =
  result = "node_" & nodeId


proc genMagicNameInCode(magicId: XfrpId): string =
  result = "magic_" & magicId


proc genFreshVariableInCode: string =
  var uid {.global.} = rand(uint16)

  result = "genvar_" & $uid
  inc uid


# code generators

proc codegenExp(env: XfrpEnv; exp: WithCodeInfo[XfrpExpr]; nameTbl: NameTables; extraVarTbl = newStringTable(); definedIn = env.name): CodeList =
  match exp.val:
    ExprLiteral(litAst):
      match litAst.val:
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
          # if id in extraVarTbl:
          #   return "" ?-> (extraVarTbl[id] & "_atlast")

          return "" ?-> (nameTbl.varNameTable[id] & "_atlast")

    ExprBin(opAsts, termAsts):
      let
        (lhsCalc, lhsResult) = env.codegenExp(termAsts[0], nameTbl, extraVarTbl)
        (rhsCalc, rhsResult) = env.codegenExp(termAsts[1], nameTbl, extraVarTbl)

        ty = env.xfrpTypeOf(exp)
        freshVar = genFreshVariableInCode()

        termTypes = termAsts.mapIt(env.xfrpTypeOf(it))

        opId = env.findOpId(opAsts[0].val, termTypes, definedIn)

      return (lhsCalc ?\ rhsCalc ?\ (?ty & " " & freshVar & " = " &
        (nameTbl.opNameTable[opKey(opId)] >?< termTypes) & "(" & lhsResult & ", " & rhsResult & ");")) ?-> freshVar

    ExprIf(ifExprAstRef, thenExprAstRef, elseExprAstRef):
      let
        (ifCalc, ifResult) = env.codegenExp(ifExprAstRef[], nameTbl, extraVarTbl)
        (thenCalc, thenResult) = env.codegenExp(thenExprAstRef[], nameTbl, extraVarTbl)
        (elseCalc, elseResult) = env.codegenExp(elseExprAstRef[], nameTbl, extraVarTbl)

        ty = env.xfrpTypeOf(exp)
        freshVar = genFreshVariableInCode()

      return (ifCalc ?\ (?ty & " " & freshVar & ";") ?\ ("if (" & ifResult & ") {") ?\
        indent(thenCalc ?\ (freshVar & " = " & thenResult & ";"), 2) ?\ "} else {" ?\
        indent(elseCalc ?\ (freshVar & " = " & elseResult & ";"), 2) ?\ "}") ?-> freshVar

    ExprApp(appIdAst, appArgAsts):
      let
        (argCalcs, argResults) = appArgAsts.mapIt(env.codegenExp(it, nameTbl, extraVarTbl)).unzip()
        resultTy = env.xfrpTypeOf(exp)
        resultFreshVar = genFreshVariableInCode()
        funcId = env.findFuncId(appIdAst.val, definedIn)

      return (argCalcs.foldl(a ?\ b, "") ?\
        (?resultTy & " " & resultFreshVar & " = " &
        nameTbl.funcNameTable[funcKey(funcId)] & "(" & argResults.join(", ") & ");")) ?-> resultFreshVar

    ExprMagic(idAst, argAsts):
      let
        (argCalcs, argResults) = argAsts.mapIt(env.codegenExp(it, nameTbl, extraVarTbl)).unzip()
        resultTy = env.xfrpTypeOf(exp)
        resultFreshVar = genFreshVariableInCode()

      return (argCalcs.foldl(a ?\ b, "") ?\
        (?resultTy & " " & resultFreshVar & " = " &
        genMagicNameInCode(idAst.val.id.val) & "(" & argResults.join(", ") & ");")) ?-> resultFreshVar


proc codegenNodeMemories(r: var Rope; env: XfrpEnv; nameTbl: NameTables) =
  r.add "/* node memories */\p"
  for nodeId in env.nodeIds:
    let ty = env.getVarType(nodeId)
    r.add ?ty & " memory_" & nameTbl.varNameTable[nodeId] & "[2];\p"
  r.add "\p"


proc codegenFuncs(r: var Rope; env: XfrpEnv; nameTbl: NameTables) =
  r.add "/* functions */\p"
  for opId in env.operatorIds:
    let opDesc = env.getOperator(opId)

    for argTypes in availableArgTypes(opDesc):
      let
        opBody = opDesc.getBody(argTypes)
        retType = opBody.retType.val

      var argsTbl = newStringTable(modeCaseSensitive)
      for arg in opBody.args:
        argsTbl[arg.val.id.val] = genArgNameInCode(arg.val.id.val)

      let envWithArgs = env.plusIdAndTypes(opBody.args.mapIt(it.val))

      let (bodyCalc, bodyResult) = envWithArgs.codegenExp(opBody.body, nameTbl, argsTbl, opId.moduleId)

      r.add "static " & ?retType & " " & (nameTbl.opNameTable[opKey(opId)] >?< argTypes) & "("
      r.add opBody.args.mapIt(?it.val.ty.val & " " & argsTbl[it.val.id.val]).join(", ")
      r.add ") {\p"
      if bodyCalc.len > 0: r.add indent(bodyCalc, 2) & "\p"
      r.add "  return " & bodyResult & ";\p}\p\p"

  for funcId in env.functionIds:
    let
      funcDesc = env.getFunction(funcId)
      (_, retType) = env.getFuncType(funcId)

    var argsTbl = newStringTable(modeCaseSensitive)
    for arg in funcDesc.args:
      argsTbl[arg.val.id.val] = genArgNameInCode(arg.val.id.val)

    let envWithArgs = env.plusIdAndTypes(funcDesc.args.mapIt(it.val))

    let (bodyCalc, bodyResult) = envWithArgs.codegenExp(funcDesc.body, nameTbl, argsTbl, funcId.module)

    r.add "static " & ?retType & " " & nameTbl.funcNameTable[funcKey(funcId)] & "("
    r.add funcDesc.args.mapIt(?it.val.ty.val & " " & argsTbl[it.val.id.val]).join(", ")
    r.add ") {\p"
    if bodyCalc.len > 0: r.add indent(bodyCalc, 2) & "\p"
    r.add "  return " & bodyResult & ";\p}\p\p"


proc codegenNodeInits(r: var Rope; env: XfrpEnv; nameTbl: NameTables) =
  r.add "/* node initializers */\p"
  for nodeId in env.nodeIds:
    let nodeDesc = env.getNode(nodeId)

    if nodeDesc.initOpt.isNone: continue

    let
      ty = env.getVarType(nodeId)
      initExprAst = nodeDesc.initOpt.get()
      (initCalc, initResult) = env.codegenExp(initExprAst, nameTbl)

    r.add "static void init_" & nameTbl.varNameTable[nodeId] & "(" & ?ty & " *output) {\p"
    if initCalc.len > 0: r.add indent(initCalc, 2) & "\p"
    r.add "  *output = " & initResult & ";\p}\p\p"


proc codegenNodeUpdates(r: var Rope; env: XfrpEnv; nameTbl: NameTables) =
  r.add "/* node updater */\p"
  for nodeId in env.innerNodeIds:
    let
      nodeDesc = env.getNode(nodeId)
      ty = env.getVarType(nodeId)
      updateExprAst = nodeDesc.update
      (updateCalc, updateResult) = env.codegenExp(updateExprAst, nameTbl)

    r.add "static void update_" & nameTbl.varNameTable[nodeId] & "("
    for refNowNodeId in nodeDesc.depsNow:
      let refNodeTy = env.getVarType(refNowNodeId)
      r.add ?refNodeTy & " " & nameTbl.varNameTable[refNowNodeId] & ", "

    for refAtLastNodeId in nodeDesc.depsAtLast:
      let refNodeTy = env.getVarType(refAtLastNodeId)
      r.add ?refNodeTy & " " & nameTbl.varNameTable[refAtLastNodeId] & "_atlast, "

    r.add ?ty & " *output) {\p"
    if updateCalc.len > 0: r.add indent(updateCalc, 2) & "\p"
    r.add "  *output = " & updateResult & ";\p}\p\p"


proc genFrpFile(env: XfrpEnv; nameTbl: NameTables): string =
  var r: Rope

  r.add "#include \""
  r.add xfrpEscape(env.name)
  r.add ".h\"\p\p"

  if "c" in env.emits:
    r.add "/* emits */\p"
    r.add env.emits["c"] & "\p"

  r.codegenNodeMemories(env, nameTbl)

  r.codegenFuncs(env, nameTbl)

  r.codegenNodeInits(env, nameTbl)

  r.codegenNodeUpdates(env, nameTbl)

  r.add "void Activate"
  r.add env.name
  r.add "(void) {\p"
  r.add "  int current_side = 0, last_side = 1;\p\p"
  r.add "  /* Initialize nodes */\p"
  for nodeId in env.nodeIds:
    let nodeDesc = env.getNode(nodeId)

    if nodeDesc.initOpt.isNone: continue

    r.add "  init_" & nameTbl.varNameTable[nodeId] & "(&memory_" & nameTbl.varNameTable[nodeId] & "[current_side]);\p"
  r.add "\p"
  r.add "XFRP_LOOP_BEGIN:\p"
  r.add "  /* Get input values */\p"
  r.add "  Input("
  r.add toSeq(env.inputNodeIds).mapIt("&memory_" & nameTbl.varNameTable[it] & "[current_side]").join(", ")
  r.add ");\p\p"
  r.add "  /* Update nodes by topologically-sorted ordering */\p"
  for nodeId in env.innerNodeIds:
    let nodeDesc = env.getNode(nodeId)

    r.add "  update_" & nameTbl.varNameTable[nodeId] & "("

    for refNowId in nodeDesc.depsNow:
      r.add "memory_" & nameTbl.varNameTable[refNowId] & "[current_side], "
    for refAtLastId in nodeDesc.depsAtLast:
      r.add "memory_" & nameTbl.varNameTable[refAtLastId] & "[last_side], "

    r.add "&memory_" & nameTbl.varNameTable[nodeId] & "[current_side]);\p"
  r.add "  /* Output values */\p"
  r.add "  Output("
  r.add toSeq(env.outputNodeIds).mapIt("&memory_" & nameTbl.varNameTable[it] & "[current_side]").join(", ")
  r.add ");\p\p"
  r.add "  /* Prepare for the next iteration */\p"
  r.add "  current_side ^= 1;\p  last_side ^= 1;\p\p"
  r.add "  goto XFRP_LOOP_BEGIN;\p"
  r.add "}\p"

  result = $r


proc genHeaderFile(env: XfrpEnv): string =
  var r: Rope

  r.add "#ifndef " & toUpperAscii(env.name) & "_H\p"
  r.add "#define " & toUpperAscii(env.name) & "_H\p\p"

  r.add xfrpTypeDefinitionCode
  r.add "\p"

  r.add "extern void Input("
  r.add toSeq(env.inputNodeIds).mapIt(?env.getVarType(it) & " *").join(", ")
  r.add ");\p"
  r.add "extern void Output("
  r.add toSeq(env.outputNodeIds).mapIt(?env.getVarType(it) & " *").join(", ")
  r.add ");\p\p"

  r.add "extern void Activate" & env.name & "();\p"

  r.add "\p#endif\p"

  result = $r


proc genMainFile(env: XfrpEnv): string =
  var r: Rope

  r.add "/* Generated by XFRP compiler */\p"

  r.add "#include \""
  r.add env.name
  r.add ".h\"\p\p"

  r.add "void Input("
  r.add toSeq(env.inputNodeIds).mapIt(?env.getVarType(it) & " *" & it).join(", ")
  r.add ") {\p  /* Your code goes here ... */\p}\p\p"
  r.add "void Output("
  r.add toSeq(env.outputNodeIds).mapIt(?env.getVarType(it) & " *" & it).join(", ")
  r.add ") {\p  /* Your code goes here ... */\p}\p\p"

  r.add "int main(int argc, char *argv[]) {\p  Activate"
  r.add env.name
  r.add "();\p  return 0;\p}\p"

  result = $r


proc codegen*(env: XfrpEnv): CCodeFiles =
  ## C code generation.
  let
    opNameTable = newStringTable(modeCaseSensitive)
    funcNameTable = newStringTable(modeCaseSensitive)
    varNameTable = newStringTable(modeCaseSensitive)
    nameTbl = (opNameTable, funcNameTable, varNameTable)

  randomize()

  for opId in env.operatorIds:
    opNameTable[opKey(opId)] = genOperatorNameInCode(opId.op)

  for funcId in env.functionIds:
    funcNameTable[funcKey(funcId)] = genFunctionNameInCode(funcId.id)

  for nodeId in env.nodeIds:
    varNameTable[nodeId] = genNodeNameInCode(nodeId)

  result.frpFile = genFrpFile(env, nameTbl)
  result.frpHeaderFile = genHeaderFile(env)
  result.mainFile = genMainFile(env)
