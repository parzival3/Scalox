package github.parzival3.lox

case class ScannerStatus(program: List[Char], start: Int, line: Int, prevTokens: List[Token])

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


  def scanTokens(program: List[Char], start: Int, current: Int, line: Int, prevTokens: List[Token]): Either[(Int, Int, Int), List[Token]] = {

//    def addToken(tokenType: TokenType, lenght: Int): ScannerStatus =
//      ScannerStatus(start = 1)
//
    def createToken(tokentype: TokenType, current: Int = 1): Token =
      Token(tokentype, completeProgram.substring(start + 1, current + 1), Something(current + 1), line)

    if program.isEmpty then Right(prevTokens.reverse)
    else program match
      case x :: xs => x match
        case '(' => scanTokens(xs, start + 1, current + 1, line, createToken(TokenType.LEFT_PAREN) :: prevTokens)
        case ')' => scanTokens(xs, start + 1, current + 1, line, createToken(TokenType.RIGHT_PAREN) :: prevTokens)
        case '{' => scanTokens(xs, start, current + 1, line, createToken(TokenType.LEFT_BRACE) :: prevTokens)
        case '}' => scanTokens(xs, start, current + 1, line, createToken(TokenType.RIGHT_BRACE) :: prevTokens)
        case ',' => scanTokens(xs, start, current + 1, line, createToken(TokenType.COMMA) :: prevTokens)
        case '.' => scanTokens(xs, start, current + 1, line, createToken(TokenType.DOT) :: prevTokens)
        case '+' => scanTokens(xs, start, current + 1, line, createToken(TokenType.PLUS) :: prevTokens)
        case ';' => scanTokens(xs, start, current + 1, line, createToken(TokenType.SEMICOLON) :: prevTokens)
        case '*' => scanTokens(xs, start, current + 1, line, createToken(TokenType.STAR) :: prevTokens)
        case '!' =>
          if xs.nextTokenIs('=') then
            scanTokens(xs.tail, start, current + 1, line, createToken(TokenType.BANG_EQUAL, 2) :: prevTokens)
          else
            scanTokens(xs, start, current + 1, line, createToken(TokenType.BANG) :: prevTokens)
        case '=' =>
          if xs.nextTokenIs('=') then
            scanTokens(xs.tail, start, current + 2, line, createToken(TokenType.EQUAL_EQUAL, 2) :: prevTokens)
          else
            scanTokens(xs, start, current + 1, line, createToken(TokenType.EQUAL) :: prevTokens)
        case '<' =>
          if xs.nextTokenIs('=') then
            scanTokens(xs.tail, start, current + 2, line, createToken(TokenType.LESS_EQUAL, 2) :: prevTokens)
          else
            scanTokens(xs, start, current + 1, line, createToken(TokenType.LESS) :: prevTokens)
        case '>' =>
          if xs.nextTokenIs('=') then
            scanTokens(xs.tail, start, current + 2, line, createToken(TokenType.GREATER_EQUAL, 2) :: prevTokens)
          else
            scanTokens(xs, start, current + 1, line, createToken(TokenType.GREATER) :: prevTokens)
        case '/' =>
          if xs.nextTokenIs('/') then
            scanTokens(xs.tail.removeCommentLine, start, current + 1, line, prevTokens)
          else
            scanTokens(xs, start, current + 1, line, createToken(TokenType.SLASH) :: prevTokens)
        case '\r' =>
          scanTokens(xs, start, current + 1, line, prevTokens)
        case ' ' =>
          scanTokens(xs, start, current + 1, line, prevTokens)
        case '\t' =>
          scanTokens(xs, start, current + 1, line, prevTokens)
        case '\n' =>
          scanTokens(xs, start, current + 1, line + 1, prevTokens)
        case '\"' =>
          xs.findMatching('\"') match
            case Some(p) =>
              val new_pos = (program.length - current) - p.length
              scanTokens(p, start, new_pos, line, createToken(TokenType.STRING, new_pos) :: prevTokens)
            case None => Left(start, current + 1, line)
        case _ => Left(start, current + 1, line)
  }