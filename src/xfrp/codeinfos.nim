from strutils import indent, align, repeat

const maxLineNumDigits = 4

type
  CodePos* = tuple
    col, line: int

  CodeLine* = string

  WithCodeInfo*[T] = object
    val: T
    startPos, endPos: CodePos
    line: CodeLine

  CodeInfo* = object
    startPos, endPos: CodePos
    line: CodeLine

func `$`*(pos: CodePos): string =
  "line: " & $pos.line & ", column: " & $succ(pos.col)

func `<`*(x, y: CodePos): bool =
  x.line < y.line or (x.line == y.line and x.col < y.col)

func pretty*(line: CodeLine; startPos, endPos: CodePos): string =
  assert(startPos < endPos)
  if startPos.line == endPos.line:
    align($startPos.line, maxLineNumDigits) & " : " & line & "\p" & indent(repeat('^', endPos.col - startPos.col), maxLineNumDigits + 3 + startPos.col)
  else:
    align($startPos.line, maxLineNumDigits) & " : " & line & "\p" & indent(repeat('^', line.len - startPos.col) & '~', maxLineNumDigits + 3 + startPos.col)

func val*[T](t: WithCodeInfo[T]): T = t.val
func startPos*[T](t: WithCodeInfo[T]): CodePos = t.startPos
func endPos*[T](t: WithCodeInfo[T]): CodePos = t.endPos
func line*[T](t: WithCodeInfo[T]): CodeLine = t.line

func attachCodeInfo*[T](val: T; startPos, endPos: CodePos; line: CodeLine): WithCodeInfo[T] =
  WithCodeInfo[T](val: val, startPos: startPos, endPos: endPos, line: line)

func codeInfo*(startPos, endPos: CodePos; line: CodeLine): CodeInfo =
  CodeInfo(startPos: startPos, endPos: endPos, line: line)

func codeInfo*[T](src: WithCodeInfo[T]): CodeInfo =
  codeInfo(src.startPos, src.endPos, src.line)

func codeInfo*(x: CodeInfo): CodeInfo = x

func pretty*(info: CodeInfo): string =
  info.line.pretty(info.startPos, info.endPos)

func pretty*[T](data: WithCodeInfo[T]): string =
  pretty(data.codeInfo)

func `..`*[T, U](x: WithCodeInfo[T]; y: WithCodeInfo[U]): CodeInfo =
  codeInfo(x.startPos, y.endPos, x.line)

func `from`*[T](data: T; info: CodeInfo): WithCodeInfo[T] =
  WithCodeInfo[T](val: data, startPos: info.startPos, endPos: info.endPos, line: info.line)

func `from`*[T, U](data: T; infoSrc: WithCodeInfo[U]): WithCodeInfo[T] =
  WithCodeInfo[T](val: data, startPos: infoSrc.startPos, endPos: infoSrc.endPos, line: infoSrc.line)
