## Materials.

import tables
from sequtils import filterIt, mapIt, insert
import syntax, codeinfos

type
  XfrpMaterials* = TableRef[XfrpModuleId, WithCodeInfo[XfrpModule]]


iterator materialsOf*(materialTbl: XfrpMaterials; moduleId: XfrpModuleId): XfrpModuleId =
  ## Iterate materials by depth-first traversal.
  var
    uses = @[moduleId]
    usesCursor = low(uses)

  while usesCursor < uses.len:
    let
      modId = uses[usesCursor]
      materialAst = materialTbl[modId]
    yield modId

    uses.insert(materialAst.val.uses.mapIt(it.val).filterIt(it notin uses), succ(usesCursor))

    inc usesCursor
