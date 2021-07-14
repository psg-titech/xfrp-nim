import patty, nimly/lextypes
from strutils import splitLines
import codeinfos

variantp XfrpRawToken:
  Ignore
  Comment(commentStr: string)
  Comma
  LBracket
  RBracket
  LParen
  RParen
  Colon
  # Semicolon
  At
  Plus
  # Minus
  # Percent
  # Asterisk
  # Slash
  # Hat
  VertVert
  # AndAnd
  # LOr
  # LAnd
  EqEq
  # NotEq
  Equal
  Lte
  # LShift
  Lt
  Rte
  # RShift
  Rt
  Module
  Material
  In
  Out
  Use
  Node
  Init
  True
  False
  If
  Then
  Else
  Last
  Function
  Id(idStr: string)
  FDigits(floatStr: string)
  Digits(intStr: string)
  Unknown

type
  XfrpToken* = object
    tok: XfrpRawToken
    startPos, endPos: CodePos
    line: CodeLine

  XfrpTokenKind* {.pure.} = XfrpRawTokenKind

func `from`*(tok: XfrpRawToken; lexTok: LToken): XfrpToken =
  let
    startPos = (col: lexTok.colNum, line: lexTok.lineNum)
    tokenStrSplitByLine = lexTok.token.splitLines()
    endPosCol = (if tokenStrSplitByLine.len == 1: startPos.col + tokenStrSplitByLine[0].len else: tokenStrSplitByLine[^1].len)
    endPosLine = startPos.line + high(tokenStrSplitByLine)
    endPos = (col: endPosCol, line: endPosLine)

  result = XfrpToken(tok: tok, startPos: startPos, endPos: endPos, line: lexTok.lineInfo.splitLines()[0])

func startPos*(tok: XfrpToken): CodePos = tok.startPos
func endPos*(tok: XfrpToken): CodePos = tok.endPos
func line*(tok: XfrpToken): CodeLine = tok.line
func kind*(tok: XfrpToken): XfrpTokenKind = tok.tok.kind

func commentStr*(tok: XfrpToken): string = tok.tok.commentStr
func idStr*(tok: XfrpToken): string = tok.tok.idStr
func floatStr*(tok: XfrpToken): string = tok.tok.floatStr
func intStr*(tok: XfrpToken): string = tok.tok.intStr
