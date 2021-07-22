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
  Gte
  # RShift
  Gt
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
  XfrpToken* = WithCodeInfo[XfrpRawToken]

  XfrpTokenKind* {.pure.} = XfrpRawTokenKind

func kind*(tok: XfrpToken): XfrpTokenKind = tok.val.kind

func commentStr*(tok: XfrpToken): string = tok.val.commentStr
func idStr*(tok: XfrpToken): string = tok.val.idStr
func floatStr*(tok: XfrpToken): string = tok.val.floatStr
func intStr*(tok: XfrpToken): string = tok.val.intStr

func `$`*(tok: XfrpToken): string =
  $tok.val & " (" & $tok.startPos & ")"
