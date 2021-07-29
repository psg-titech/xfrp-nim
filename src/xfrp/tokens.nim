## XFRP tokens.

import patty
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
  At
  Slash # for path
  Equal
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
  InfixLeft
  InfixRight
  InfixNone
  Magic
  Emit
  Id(idStr: string)
  Operator(opStr: string)
  FDigits(floatStr: string)
  Digits(intStr: string)
  TripleQuoted(tqStr: string)
  Unknown

type
  XfrpToken* = WithCodeInfo[XfrpRawToken]

  XfrpTokenKind* {.pure.} = XfrpRawTokenKind

func kind*(tok: XfrpToken): XfrpTokenKind = tok.val.kind

func commentStr*(tok: XfrpToken): string = tok.val.commentStr
func idStr*(tok: XfrpToken): string = tok.val.idStr
func opStr*(tok: XfrpToken): string = tok.val.opStr
func floatStr*(tok: XfrpToken): string = tok.val.floatStr
func intStr*(tok: XfrpToken): string = tok.val.intStr
func tqStr*(tok: XfrpToken): string = tok.val.tqStr

func `$`*(tok: XfrpToken): string =
  $tok.val & " (" & $tok.startPos & ")"
