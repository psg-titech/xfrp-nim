## XFRP lexer.

from strutils import splitLines
import nimly
import tokens, codeinfos

converter lexTokenToCodeInfo(lexTok: LToken): CodeInfo =
  # Implicitly convert nimly's token (LToken) to a code information (CodeInfo).
  let
    startPos = (col: lexTok.colNum, line: lexTok.lineNum)
    tokenStrSplitByLine = lexTok.token.splitLines()
    endPosCol = (if tokenStrSplitByLine.len == 1: startPos.col + tokenStrSplitByLine[0].len else: tokenStrSplitByLine[^1].len)
    endPosLine = startPos.line + high(tokenStrSplitByLine)
    endPos = (col: endPosCol, line: endPosLine)

  result = codeInfo(startPos, endPos, lexTok.lineInfo.splitLines()[0])

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
  r"@":
    return At() from token
  r"/":
    return Slash() from token
  r"=":
    return Equal() from token
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
  r"infixl":
    return InfixLeft() from token
  r"infixr":
    return InfixRight() from token
  r"infix":
    return InfixNone() from token
  r"magic":
    return Magic() from token
  r"emit":
    return Emit() from token
  r"[A-Za-z_][A-Za-z0-9_]*":
    return Id(token.token) from token
  r"[!#$%&*+./<=>?@\\^|\-~]+":
    return Operator(token.token) from token
  r"(0|[1-9][0-9]*)\.[0-9]+":
    return FDigits(token.token) from token
  r"0|[1-9][0-9]*":
    return Digits(token.token) from token
  "\"\"\"([^\"]|\"[^\"]|\"\"[^\"])*\"\"\"":
    return TripleQuoted(token.token) from token

type
  XfrpLexer* = NimlLexer[XfrpToken]


proc buildLexerFromFilename*(filename: string): XfrpLexer =
  result = xfrpLex.open(filename)


proc buildLexerFromString*(str: string): XfrpLexer =
  result = xfrpLex.newWithString(str)
