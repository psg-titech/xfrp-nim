from strutils import join
import patty

variantp XfrpType:
  TUnit
  TBool
  TInt
  TFloat
  TTuple(tupleParams: seq[XfrpType])

func `$`*(ty: XfrpType): string =
  match ty:
    TUnit: return "Unit"
    TBool: return "Bool"
    TInt: return "Int"
    TFloat: return "Float"
    TTuple(params): return "(" & params.join(", ") & ")"
