import nimly
import tokens

niml xfrpLex[XfrpToken]:
  r"\s+":
    return Ignore() from token
  "#[^\n]*":
    return Comment(token.token) from token
  r",":
    return Comma() from token
  r"\[":
    return LBracket() from token
  r"]":
    return RBracket() from token
  r"\(":
    return LParen() from token
  r"\)":
    return RParen() from token
  r":":
    return Colon() from token
  # r";":
  #   return Semicolon() from token
  r"@":
    return At() from token
  r"\+":
    return Plus() from token
  # r"-":
  #   return Minus() from token
  # r"%":
  #   return Percent() from token
  # r"*":
  #   return Asterisk() from token
  # r"/":
  #   return Slash() from token
  # r"^":
  #   return Hat() from token
  r"\|\|":
    return VertVert() from token
  # r"&&":
  #   return AndAnd() from token
  # r"|":
  #   return LOr() from token
  # r"&":
  #   return LAnd() from token
  r"==":
    return EqEq() from token
  # r"!=":
  #   return NotEq() from token
  r"=":
    return Equal() from token
  r"<=":
    return Lte() from token
  # r"<<":
  #   return LShift() from token
  r"<":
    return Lt() from token
  r">=":
    return Rte() from token
  # r">>":
  #   return RShift() from token
  r">":
    return Rt() from token
  r"module":
    return Module() from token
  r"material":
    return Material() from token
  r"in":
    return In() from token
  r"out":
    return Out() from token
  r"use":
    return Use() from token
  r"node":
    return Node() from token
  r"init":
    return Init() from token
  r"true":
    return True() from token
  r"false":
    return False() from token
  r"if":
    return If() from token
  r"then":
    return Then() from token
  r"else":
    return Else() from token
  r"last":
    return Last() from token
  r"func(tion)?":
    return Function() from token
  r"[A-Za-z_][A-Za-z0-9_]*":
    return Id(token.token) from token
  r"(0|[1-9][0-9]*)\.[0-9]+":
    return FDigits(token.token) from token
  r"0|[1-9][0-9]*":
    return Digits(token.token) from token

when isMainModule:
  import os
  import codeinfos

  var l = xfrpLex.open(paramStr(1))
  for token in l.lexIter:
    echo token.kind
    if token.kind != XfrpTokenKind.Ignore:
      echo pretty(token.line, token.startPos, token.endPos)
