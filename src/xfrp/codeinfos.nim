## Additional information about XFRP codes.

from strutils import indent, align, repeat

const maxLineNumDigits = 4

type
  CodePos* = tuple
    ## Location in a code.
    col, line: int

  CodeLine* = string

  WithCodeInfo*[T] = object
    ## A value and the code fragment which represents the value.
    ## It must satisfy `startPos < endPos`.
    val: T
    startPos, endPos: CodePos
    line: CodeLine

  CodeInfo* = object
    ## A code fragment.
    ## It must satisfy ``startPos < endPos``.
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


# getter functons

func val*[T](t: WithCodeInfo[T]): T = t.val
func startPos*[T](t: WithCodeInfo[T]): CodePos = t.startPos
func endPos*[T](t: WithCodeInfo[T]): CodePos = t.endPos
func line*[T](t: WithCodeInfo[T]): CodeLine = t.line


func attachCodeInfo*[T](val: T; info: CodeInfo): WithCodeInfo[T] =
  ## Attach a code fragment to a value.
  WithCodeInfo[T](val: val, startPos: info.startPos, endPos: info.endPos, line: info.line)


func codeInfo*(startPos, endPos: CodePos; line: CodeLine): CodeInfo =
  ## Construct a code fragment.
  assert(startPos < endPos)
  CodeInfo(startPos: startPos, endPos: endPos, line: line)


func codeInfo*[T](src: WithCodeInfo[T]): CodeInfo =
  ## Extract a code fragment from a value with code information.
  codeInfo(src.startPos, src.endPos, src.line)


func codeInfo*(x: CodeInfo): CodeInfo =
  ## Extract a code fragment.
  ## This function is actually the identity function.
  x


func pretty*(info: CodeInfo): string =
  info.line.pretty(info.startPos, info.endPos)


func pretty*[T](data: WithCodeInfo[T]): string =
  pretty(data.codeInfo)


func `..`*[T, U](x: WithCodeInfo[T]; y: WithCodeInfo[U]): CodeInfo =
  ## Construct a code fragment from ``x`` to ``y``.
  codeInfo(x.startPos, y.endPos, x.line)


func `from`*[T](data: T; info: CodeInfo): WithCodeInfo[T] =
  ## The same as ``attachCodeInfo(data, info)``.
  ##
  ## **See also:**
  ## * `attachCodeInfo <#attachCodeInfo,T,CodeInfo>`_
  attachCodeInfo(data, info)


func `from`*[T, U](data: T; infoSrc: WithCodeInfo[U]): WithCodeInfo[T] =
  ## The same as ``attachCodeInfo(data, infoSrc.codeInfo)``.
  ##
  ## **See also:**
  ## * `attachCodeInfo <#attachCodeInfo,T,CodeInfo>`_
  ## * `codeInfo <#codeInfo,WithCodeInfo[T]>`_
  attachCodeInfo(data, infoSrc.codeInfo)
