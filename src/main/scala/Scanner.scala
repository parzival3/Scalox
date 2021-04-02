package github.parzival3.lox


case class ScannerState(program: List[Char], start: Int, line: Int, prevTokens: List[Token])

class Scanner(completeProgram: String):
  extension (program: List[Char])
    def nextTokenIs(char: Char): Boolean =
      program match
        case x :: xs =>
          if (x == char) then true
          else if x != ' ' then
            xs nextTokenIs (char)
          else
            false
        case _ => false

  extension (program: List[Char])
    def removeCommentLine: List[Char] =
      program match
        case x :: xs =>
          // TODO : check for null termination and scala strings
          // we need to return the full program in order to keep the count of line
          if x != '\n' then
            xs.removeCommentLine
          else
            program
        case nil => program

  extension (program: List[Char])
    def findMatching(char: Char): Option[List[Char]] =
      program match
        case x :: xs =>
          if x == '\n' || x == '\r' then
            None
          else if x == char then
            Some(xs)
          else
            xs findMatching (char)
        case _ => None

  extension (program: List[Char])
    def findDigit: Option[List[Char]] =
      program match
        case x :: xs =>
          if x.isDigit || x == '.' then
            xs.findDigit
          else if x.isWhitespace || x == '\n' || x == '\r' then
            Some(xs)
          else
            None
        case _ => Some(program)


  def scanTokens(status: ScannerState): Either[(Int, Int), List[Token]] =

    def createToken(tokentype: TokenType, string: String, line: Int): Token =
      tokentype match
        case TokenType.STRING => Token(tokentype, string, Some(string), line)
        case TokenType.NUMBER => Token(tokentype, string, Some(string.toDouble), line)
        case _  => Token(tokentype, string, None, line)

    def updateState(restProgram: List[Char], tokentype: Option[TokenType], line: Int): ScannerState =
      val size = status.program.length - restProgram.length
      tokentype match
        case Some(t) =>
          val newToken = createToken(t, completeProgram.substring(status.start, status.start + size), line)
          ScannerState(restProgram, status.start + size, line, newToken :: status.prevTokens)
        case None => ScannerState(restProgram, status.start + size, line, status.prevTokens)

    if status.program.isEmpty then Right(status.prevTokens.reverse)
    else status.program match
      case x :: xs => x match
        case '(' => scanTokens(updateState(xs, Some(TokenType.LEFT_PAREN), status.line))
        case ')' => scanTokens(updateState(xs, Some(TokenType.RIGHT_PAREN), status.line))
        case '{' => scanTokens(updateState(xs, Some(TokenType.LEFT_BRACE), status.line))
        case '}' => scanTokens(updateState(xs, Some(TokenType.RIGHT_BRACE), status.line))
        case ',' => scanTokens(updateState(xs, Some(TokenType.COMMA), status.line))
        case '.' => scanTokens(updateState(xs, Some(TokenType.DOT), status.line))
        case '+' => scanTokens(updateState(xs, Some(TokenType.PLUS), status.line))
        case ';' => scanTokens(updateState(xs, Some(TokenType.SEMICOLON), status.line))
        case '*' => scanTokens(updateState(xs, Some(TokenType.STAR), status.line))
        case '!' =>
          if xs.nextTokenIs('=') then
            scanTokens(updateState(xs.tail, Some(TokenType.BANG_EQUAL), status.line))
          else
            scanTokens(updateState(xs, Some(TokenType.BANG), status.line))
        case '=' =>
          if xs.nextTokenIs('=') then
            scanTokens(updateState(xs.tail, Some(TokenType.EQUAL_EQUAL), status.line))
          else
            scanTokens(updateState(xs, Some(TokenType.EQUAL), status.line))
        case '<' =>
          if xs.nextTokenIs('=') then
            scanTokens(updateState(xs.tail, Some(TokenType.LESS_EQUAL), status.line))
          else
            scanTokens(updateState(xs, Some(TokenType.LESS), status.line))
        case '>' =>
          if xs.nextTokenIs('=') then
            scanTokens(updateState(xs.tail, Some(TokenType.GREATER_EQUAL), status.line))
          else
            scanTokens(updateState(xs, Some(TokenType.GREATER), status.line))
        case '/' =>
          if xs.nextTokenIs('/') then
            scanTokens(updateState(xs.tail.removeCommentLine, None, status.line))
          else
            scanTokens(updateState(xs, Some(TokenType.SLASH), status.line))
        case '\r' =>
          scanTokens(updateState(xs, None, status.line))
        case ' ' =>
          scanTokens(updateState(xs, None, status.line))
        case '\t' =>
          scanTokens(updateState(xs, None, status.line))
        case '\n' =>
          scanTokens(updateState(xs, None, status.line + 1))
        case '\"' =>
          xs.findMatching('\"') match
            case Some(p) =>
              scanTokens(updateState(p, Some(TokenType.STRING), status.line))
            case None => Left(status.start, status.line)
        case '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '0' =>
          xs.findDigit match
            case Some(d) =>
              scanTokens(updateState(d, Some(TokenType.NUMBER), status.line))
            case None => Left(status.start, status.line)

        case _ => Left(status.start, status.line)
