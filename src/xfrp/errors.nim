type
  XfrpLanguageError* = object of CatchableError

  XfrpSyntaxError* = object of XfrpLanguageError
  XfrpTypeError* = object of XfrpLanguageError
