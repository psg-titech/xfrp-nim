## XFRP environments.
## The environment is actually the combination of four specialized environments with several additional data about the program.
##
## **See also:**
## * `envs/operators <envs/operators.html>`_ for operator environments
## * `envs/functions <envs/functions.html>`_ for function environments
## * `envs/nodes <envs/ndoes.html>`_ for node environments
## * `envs/typecheck <envs/typecheck.html>`_ for type environments and XFRP type system

import strtabs
import syntax, materials, codeinfos, types
import envs/[operators, functions, nodes, typecheck]

export operators.XfrpOpId
export functions.XfrpFuncId
export nodes.XfrpNodeId

export operators.XfrpOpDescription, operators.availableArgTypes, operators.getBody
export operators.XfrpOpBody, operators.args, operators.retType, operators.body
export functions.XfrpFuncDescription, functions.args, functions.body
export nodes.XfrpNodeDescription, nodes.initOpt, nodes.update, nodes.depsNow, nodes.depsAtLast

export typecheck.XfrpFuncType

type
  XfrpEnv* = tuple
    ## An environment.
    name: XfrpModuleId
    materials: XfrpMaterials
    opEnv: XfrpOpEnv
    funcEnv: XfrpFuncEnv
    nodeEnv: XfrpNodeEnv
    tyEnv: XfrpTypeEnv
    emits: StringTableRef


proc makeEnvironment*(materials: XfrpMaterials): XfrpEnv =
  ## Construct new environment from materials.
  result.name = materials.getRootId()
  result.materials = materials
  result.opEnv = makeOperatorEnvironment(materials)
  result.funcEnv = makeFunctionEnvironment(materials, result.opEnv)
  result.nodeEnv = makeNodeEnvironment(materials, result.opEnv)

  for exp in exprs(result.nodeEnv):
    result.funcEnv.checkFuncValidity(exp, materials.getRootId(), materials)

  result.tyEnv = makeTypeEnvironment(materials, result.opEnv, result.funcEnv, result.nodeEnv)

  result.emits = newStringTable(modeCaseInsensitive)
  for moduleId in materials:
    for emitAst in materials[moduleId].val.emits:
      let (target, code) = emitAst.val
      if target in result.emits:
        result.emits[target] &= code

      else:
        result.emits[target] = code


iterator operatorIds*(env: XfrpEnv): XfrpOpId =
  for id in env.opEnv: yield id


iterator functionIds*(env: XfrpEnv): XfrpFuncId =
  for id in env.funcEnv: yield id


iterator nodeIds*(env: XfrpEnv): XfrpNodeId =
  for id in env.nodeEnv: yield id


iterator innerNodeIds*(env: XfrpEnv): XfrpNodeId =
  for id in innerNodeIds(env.nodeEnv): yield id


iterator inputNodeIds*(env: XfrpEnv): XfrpNodeId =
  for id in inputNodeIds(env.nodeEnv): yield id


iterator outputNodeIds*(env: XfrpEnv): XfrpNodeId =
  for id in outputNodeIds(env.nodeEnv): yield id


proc getVarType*(env: XfrpEnv; id: XfrpNodeId): XfrpType =
  result = env.tyEnv.getVarType(id)


proc getFuncType*(env: XfrpEnv; id: XfrpFuncId): XfrpFuncType =
  result = env.tyEnv.getFuncType(id)


proc getOperator*(env: XfrpEnv; id: XfrpOpId): XfrpOpDescription =
  env.opEnv.getOperator(id)


proc getFunction*(env: XfrpEnv; id: XfrpFuncId): XfrpFuncDescription =
  env.funcEnv.getFunction(id)


proc getNode*(env: XfrpEnv; id: XfrpNodeId): XfrpNodeDescription =
  env.nodeEnv.getNode(id)


proc plusIdAndTypes*(env: XfrpEnv; idAndTypes: seq[XfrpIdAndType]): XfrpEnv =
  result = env
  for idAndType in idAndTypes:
    result.tyEnv.addVar(idAndType)


proc xfrpTypeOf*(env: XfrpEnv; exp: WithCodeInfo[XfrpExpr]; definedIn = env.name): XfrpType =
  env.tyEnv.xfrpTypeOf(exp, definedIn, env.materials)


proc findOpId*(env: XfrpEnv; op: XfrpOperator; termTypes: seq[XfrpType]; definedIn: XfrpModuleId): XfrpOpId =
  env.opEnv.findOpId(op, termTypes, definedIn, env.materials)


proc findFuncId*(env: XfrpEnv; id: XfrpId; definedIn: XfrpModuleId): XfrpFuncId =
  env.funcEnv.findFuncId(id, definedIn, env.materials)
