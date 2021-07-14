from strutils import indent, align, repeat

const maxLineNumDigits = 4

type
  CodePos* = tuple
    col, line: int

  CodeLine* = string

func `$`*(pos: CodePos): string =
  "line: " & $pos.line & ", column: " & $pos.col

func `<`*(x, y: CodePos): bool =
  x.line < y.line or (x.line == y.line and x.col < y.col)

func pretty*(line: CodeLine; startPos, endPos: CodePos): string =
  assert(startPos < endPos)
  if startPos.line == endPos.line:
    align($startPos.line, maxLineNumDigits) & " : " & line & "\p" & indent(repeat('^', endPos.col - startPos.col), maxLineNumDigits + 3 + startPos.col)
  else:
    align($startPos.line, maxLineNumDigits) & " : " & line & "\p" & indent(repeat('^', line.len - startPos.col) & '~', maxLineNumDigits + 3 + startPos.col)
