## XFRP primitive types.
##
## **See also:**
## * <typecheck `envs/typecheck.html>`_ for type environments and XFRP type system

import hashes
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
  ## Short representation of types.
  ## It is useful for naming overloaded operators uniquely.
  match ty:
    TBool: return "B"
    TInt: return "I"
    TFloat: return "F"

proc hash*(x: XfrpType): Hash =
  var h: Hash
  h = h !& hash($x)
  result = !$h
