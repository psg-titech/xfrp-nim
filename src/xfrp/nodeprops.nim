from sequtils import toSeq, allIt, filterIt, mapIt

type
  ReferencesOf*[T] = (proc (x: T): seq[T] {.noSideEffect.})
  ReferenceGraph*[T] = tuple
    domain: seq[T]
    referencesOf: ReferencesOf[T]


func getAnyReferenceCycle[T](graph: ReferenceGraph[T]; x: T; trace: seq[T] = @[]): seq[T] =
  if trace.len > 0 and x == trace[0]:
    # cycle (x -> ... -> x) detected
    return trace

  elif x in trace:
    # cycle is detected, but the loop begins from the midpoint
    return

  for y in graph.referencesOf(x):
    if y notin graph.domain: continue

    let cycle = graph.getAnyReferenceCycle(y, trace & x)
    if cycle.len > 0:
      # a cycle has already been detected
      return cycle


func getAnyReferenceCycle*[T](graph: ReferenceGraph[T]): seq[T] =
  ## Check if the reference graph has any cycles.
  ## If a cycle is found, then return it (result.len > 0).
  for x in graph.domain:
    let cycle = graph.getAnyReferenceCycle(x)
    if cycle.len > 0:
      return cycle


func topologicallySorted*[T](graph: ReferenceGraph[T]; initVisited: seq[T] = @[]): seq[T] =
  ## Apply topological sorting for the given graph.
  ## You should check the graph has no cycle before calling it.
  assert(graph.getAnyReferenceCycle().len == 0)

  func isAllRefIn(x: T; xs: seq[T]): bool =
    graph.referencesOf(x).allIt(it in xs)

  var
    visited = initVisited
    accessibles = graph.domain.filterIt(it.isAllRefIn(visited))
    idx = low(accessibles)

  while accessibles.len > idx:
    let x = accessibles[idx]
    inc idx
    visited.add x

    accessibles.add graph.domain.filterIt(it notin accessibles).filterIt(it.isAllRefIn(visited))

    result.add x


when isMainModule:
  import os
  from strutils import join
  import lexer, parser, envs, errors, codeinfos

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
