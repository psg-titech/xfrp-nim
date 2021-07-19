from strutils import join
from sequtils import toSeq, allIt, filterIt, mapIt
import envs, errors, codeinfos


func getAnyReferenceCycle(env: XfrpEnv; n: XfrpId; trace: seq[XfrpId] = @[]): seq[XfrpId] =
  if trace.len > 0 and n == trace[0]:
    return trace

  elif n in trace: return

  for dependedNode in env.getInnerNode(n).refNow:
    if env.isInputNode(dependedNode):
      continue

    let refCycle = env.getAnyReferenceCycle(dependedNode, trace & n)
    if refCycle.len > 0:
      return refCycle


func getAnyReferenceCycle(env: XfrpEnv): seq[XfrpId] =
  # check if the environment has any reference cycles
  # if a cycle is found, then return it (len > 0)
  for n in env.innerNodeIds:
    let refCycle = env.getAnyReferenceCycle(n)
    if refCycle.len > 0:
      return refCycle


proc getTopologicallySortedNodeList*(env: XfrpEnv): seq[XfrpId] =
  # return node ID list by topologically-sorterd ordering
  let referenceCycle = env.getAnyReferenceCycle()
  if referenceCycle.len > 0:
    let
      referenceCycleDiagram = (referenceCycle & referenceCycle[0]).join(" -> ")
      err = XfrpReferenceError.newException("A cycle reference is detected. At-last reference is useful for avoiding such a cycle reference. (" & referenceCycleDiagram & ")")
    for nodeId in referenceCycle:
      err.causedBy(env.getInnerNode(nodeId).id)
    raise err

  func allRefNowIncludes(n: XfrpNodeDefinition; ns: openArray[XfrpId]): bool =
    refNow(n).allIt(it in ns)

  let innerNodes = toSeq(env.innerNodeIds)
  var
    evaluated = toSeq(env.inputNodeIds) # input nodes are already evaluated
    evaluables = innerNodes.mapIt(env.getInnerNode(it)).filterIt(it.allRefNowIncludes(evaluated)).mapIt(it.id.val)
    idx = low(evaluables)

  while evaluables.len > idx:
    let n = evaluables[idx]
    inc idx
    evaluated.add n

    evaluables.add innerNodes
      .filterIt(it notin evaluables)
      .mapIt(env.getInnerNode(it))
      .filterIt(it.allRefNowIncludes(evaluated))
      .mapIt(it.id.val)

    result.add n


when isMainModule:
  import os
  import lexer, parser

  if paramCount() < 1:
    stderr.writeLine "usage: nodeprops [filename]"
    quit QuitFailure

  var l = buildLexerFromFilename(paramStr(1))

  try:
    let
      ast = parse(l)
      env = makeEnvironmentFromModule(ast.val)
      sortedNodes = env.getTopologicallySortedNodeList()

    echo sortedNodes

  except XfrpLanguageError as err:
    stderr.writeLine "[", err.name, "] ", err.msg
    for info in err.causes:
      stderr.writeLine pretty(info)
