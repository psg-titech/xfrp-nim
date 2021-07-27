## XFRP primitive types.

import patty

variantp XfrpType:
  TBool
  TInt
  TFloat

func `$`*(ty: XfrpType): string =
  match ty:
    TBool: return "Bool"
    TInt: return "Int"
    TFloat: return "Float"

func short*(ty: XfrpType): string =
  match ty:
    TBool: return "B"
    TInt: return "I"
    TFloat: return "F"
