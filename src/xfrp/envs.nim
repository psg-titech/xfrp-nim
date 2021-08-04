# XFRP environments.

import syntax, materials, codeinfos
import envs/[operators, functions, nodes, typecheck]


type
  XfrpEnv* = object
    materials: XfrpMaterials
    opEnv: XfrpOpEnv
    funcEnv: XfrpFuncEnv
    nodeEnv: XfrpNodeEnv
    tyEnv: XfrpTypeEnv


proc makeEnvironment*(materials: XfrpMaterials): XfrpEnv =
  result.materials = materials
  result.opEnv = makeOperatorEnvironmentFromModule(materials)
  result.funcEnv = makeFunctionEnvironment(materials, result.opEnv)
  result.nodeEnv = makeNodeEnvironment(materials, result.opEnv)

  for exp in exprs(result.nodeEnv):
    result.funcEnv.checkFuncValidity(exp, materials.getRoot().val.moduleId.val, materials)

  result.tyEnv = makeTypeEnvironment(materials, result.opEnv, result.funcEnv, result.nodeEnv)
