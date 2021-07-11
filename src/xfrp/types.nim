import patty

variantp XfrpType:
  TUnit
  TBool
  TInt
  TFloat
  TTuple(tupleParams: seq[ref XfrpType])
