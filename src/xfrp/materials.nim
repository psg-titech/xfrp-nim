## Materials.

import tables
from sequtils import filterIt, mapIt, insert
import syntax, codeinfos

type
  XfrpMaterials* = object
    tbl: TableRef[XfrpModuleId, WithCodeInfo[XfrpModule]]
    root: XfrpModuleId


proc makeMaterials*(tbl: TableRef[XfrpModuleId, WithCodeInfo[XfrpModule]]; root: XfrpModuleId): XfrpMaterials =
  assert(root in tbl)
  result = XfrpMaterials(tbl: tbl, root: root)


proc `[]`*(materialTbl: XfrpMaterials; id: XfrpModuleId): WithCodeInfo[XfrpModule] =
  result = materialTbl.tbl[id]


proc getRoot*(materialTbl: XfrpMaterials): WithCodeInfo[XfrpModule] =
  result = materialTbl[materialTbl.root]


iterator materialsOf*(materialTbl: XfrpMaterials; moduleId: XfrpModuleId): XfrpModuleId =
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

iterator pairs*(materialTbl: XfrpMaterials): (XfrpModuleId, WithCodeInfo[XfrpModule]) =
  for idAndModulePair in pairs(materialTbl.tbl):
    yield idAndModulePair
