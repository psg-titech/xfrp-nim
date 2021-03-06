## Materials.

import tables
from sequtils import filterIt, mapIt, insert
import syntax, codeinfos

type
  XfrpMaterials* = object
    ## Materials.
    ## The root module is usually a normal module, not a material module.
    tbl: TableRef[XfrpModuleId, WithCodeInfo[XfrpModule]]
    root: XfrpModuleId


proc makeMaterials*(tbl: TableRef[XfrpModuleId, WithCodeInfo[XfrpModule]]; root: XfrpModuleId): XfrpMaterials =
  ## Construct new materials.
  assert(root in tbl)
  result = XfrpMaterials(tbl: tbl, root: root)


proc `[]`*(materialTbl: XfrpMaterials; id: XfrpModuleId): WithCodeInfo[XfrpModule] =
  ## Get a material AST from given module ID.
  result = materialTbl.tbl[id]


proc getRoot*(materialTbl: XfrpMaterials): WithCodeInfo[XfrpModule] =
  ## Get a root module AST.
  result = materialTbl[materialTbl.root]


proc getRootId*(materialTbl: XfrpMaterials): XfrpModuleId =
  ## Get a root module ID.
  result = materialTbl.root


iterator materialsOf*(materialTbl: XfrpMaterials; moduleId: XfrpModuleId): XfrpModuleId =
  ## Iterate materials of given module by ID.
  yield moduleId

  let materialAst = materialTbl.tbl[moduleId]
  for idAst in materialAst.val.uses:
    yield idAst.val


iterator depthFirst*(materialTbl: XfrpMaterials; moduleId: XfrpModuleId): XfrpModuleId =
  ## Iterate materials by depth-first traversal.
  var
    uses = @[moduleId]
    usesCursor = low(uses)

  while usesCursor < uses.len:
    let
      modId = uses[usesCursor]
      materialAst = materialTbl.tbl[modId]
    yield modId

    uses.insert(materialAst.val.uses.mapIt(it.val).filterIt(it notin uses), succ(usesCursor))

    inc usesCursor


iterator items*(materialTbl: XfrpMaterials): XfrpModuleId =
  ## Iterate all available materials.
  for id in materialTbl.depthFirst(materialTbl.root):
    yield id


iterator pairs*(materialTbl: XfrpMaterials): (XfrpModuleId, WithCodeInfo[XfrpModule]) =
  ## Iterate materials with its own IDs.
  for idAndModulePair in pairs(materialTbl.tbl):
    yield idAndModulePair
