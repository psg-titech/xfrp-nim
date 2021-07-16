import codeinfos

type
  XfrpLanguageError* = object of CatchableError
    causes: seq[CodeInfo]

  XfrpSyntaxError* = object of XfrpLanguageError
    ## An error about lexing and parsing

  XfrpTypeError* = object of XfrpLanguageError
    ## An error about type system

  XfrpDefinitionError* = object of XfrpLanguageError
    ## An error about constructing nodes, functions and any other definitions.

func causes*(e: ref XfrpLanguageError): seq[CodeInfo] =
  e.causes

proc causedBy*(e: ref XfrpLanguageError; cs: varargs[CodeInfo, codeInfo]) =
  for c in cs:
    e.causes.add c
