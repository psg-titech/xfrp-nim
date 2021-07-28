## XFRP errors.

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

  XfrpReferenceError* = object of XfrpLanguageError
    ## An error about references

  XfrpLoadError* = object of XfrpLanguageError
    ## An error while loading files.
    searchPaths*: seq[string]

proc causedBy*(e: ref XfrpLanguageError; cs: varargs[CodeInfo, codeInfo]) =
  ## Register erroneous codes.
  for c in cs:
    e.causes.add c

iterator causes*(e: ref XfrpLanguageError): CodeInfo =
  for c in e.causes:
    yield c
