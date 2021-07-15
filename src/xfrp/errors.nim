import codeinfos

type
  XfrpLanguageError* = object of CatchableError
    causes: seq[CodeInfo]

  XfrpSyntaxError* = object of XfrpLanguageError
  XfrpTypeError* = object of XfrpLanguageError

func causes*(e: ref XfrpLanguageError): seq[CodeInfo] =
  e.causes

proc causedBy*(e: ref XfrpLanguageError; cs: varargs[CodeInfo, codeInfo]) =
  for c in cs:
    e.causes.add c
