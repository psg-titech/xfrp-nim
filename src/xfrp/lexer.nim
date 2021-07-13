import nimly

import tokens

niml xfrpLex[XfrpToken]:
  r"\s+":
    return Ignore()
  "#[^\n]*":
    return Comment(token.token)
  r",":
    return Comma()
  r"\[":
    return LBracket()
  r"]":
    return RBracket()
  r"\(":
    return LParen()
  r"\)":
    return RParen()
  r":":
    return Colon()
  # r";":
  #   return Semicolon()
  r"@":
    return At()
  r"\+":
    return Plus()
  # r"-":
  #   return Minus()
  # r"%":
  #   return Percent()
  # r"*":
  #   return Asterisk()
  # r"/":
  #   return Slash()
  # r"^":
  #   return Hat()
  r"\|\|":
    return VertVert()
  # r"&&":
  #   return AndAnd()
  # r"|":
  #   return LOr()
  # r"&":
  #   return LAnd()
  r"==":
    return EqEq()
  # r"!=":
  #   return NotEq()
  r"=":
    return Equal()
  r"<=":
    return Lte()
  # r"<<":
  #   return LShift()
  r"<":
    return Lt()
  r">=":
    return Rte()
  # r">>":
  #   return RShift()
  r">":
    return Rt()
  r"module":
    return Module()
  r"in":
    return In()
  r"out":
    return Out()
  r"use":
    return Use()
  r"node":
    return Node()
  r"init":
    return Init()
  r"true":
    return True()
  r"false":
    return False()
  r"if":
    return If()
  r"then":
    return Then()
  r"else":
    return Else()
  r"last":
    return Last()
  r"function":
    return Function()
  r"[A-Za-z_][A-Za-z0-9_]*":
    return Id(token.token)
  r"(0|[1-9][0-9]*)\.[0-9]+":
    return FDigits(token.token)
  r"0|[1-9][0-9]*":
    return Digits(token.token)

when isMainModule:
  import os
  var l = xfrpLex.open(paramStr(1))
  for token in l.lexIter:
    echo token
