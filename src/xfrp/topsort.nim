## Generic topological sorting algorithm.

from sequtils import toSeq, allIt, filterIt, mapIt

type
  ReferencesOf*[T] = (proc (x: T): seq[T] {.noSideEffect.})
  ReferenceGraph*[T] = tuple
    ## Directed Acyclic Graph (DAG).
    domain: seq[T] ## Nodes.
    referencesOf: ReferencesOf[T] ## Directed edges.


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
