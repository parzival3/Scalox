package github.parzival3.lox
import scala.annotation.tailrec


case class ScannerState(program: List[Char], start: Int, line: Int, prevTokens: List[Token])

class Scanner(completeProgram: String):
  extension (program: List[Char])
    @tailrec
    final def nextTokenIs(char: Char): Boolean =
      program match
        case x :: xs =>
          if (x == char) then true
          else if x != ' ' then
            xs nextTokenIs (char)
          else
            false
        case _ => false

    @tailrec
    final def removeCommentLine: List[Char] =
      program match
        case x :: xs =>
          // TODO : check for null termination and scala strings
          // we need to return the full program in order to keep the count of line
          if x != '\n' then
            xs.removeCommentLine
          else
            program
        case nil => program

    @tailrec
    final def findMatching(char: Char): Option[List[Char]] =
      program match
        case x :: xs =>
          if x == '\n' || x == '\r' then
            None
          else if x == char then
            Some(xs)
          else
            xs findMatching (char)
        case _ => None

    @tailrec
    final def findIdentifier: Option[List[Char]] =
      program match
        case x :: xs =>
          if x.isDigit || x.isLetter || x == '_' then
            xs.findIdentifier
          else
            None
        case Nil =>
            Some(Nil)

    def findDigit: Option[List[Char]] =
      var hasDecimal = false
      var digitAfterPoint = false

      @tailrec
      def findEnd(string: List[Char]): Option[List[Char]] =
        string match
          case x :: xs => x match
            case '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '0' =>
                if hasDecimal then
                  digitAfterPoint = true
                findEnd(xs)
            case '.' =>
                if hasDecimal then
                    None
                else
                    hasDecimal = true
                    findEnd(xs)
            case '\n' | '\r' | ' ' =>
                if  (hasDecimal && digitAfterPoint)  then
                  Some(xs)
                else if hasDecimal then
                  None
                else
                  Some(string)
          case Nil =>
                  // TODO : we shouldn't be here
                  Some(string)
      findEnd(program)


  @tailrec
  final def scanTokens(status: ScannerState): Either[(Int, Int), List[Token]] =
    object Digit:
        def unapply(c: Char): Boolean = c.isDigit
    object Letter:
        def unapply(c: Char): Boolean = c.isLetter

    def createToken(tokentype: TokenType, string: String, line: Int): Token =
      tokentype match
        case TokenType.STRING => Token(tokentype, string, Some(string), line)
        case TokenType.NUMBER => Token(tokentype, string, Some(string.toDouble), line)
        case TokenType.IDENTIFIER => Token(Keywords.kmap.getOrElse(string, TokenType.IDENTIFIER), string, None, line)
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
        case '\r'| ' ' | '\t'  =>
          scanTokens(updateState(xs, None, status.line))
        case '\n' =>
          scanTokens(updateState(xs, None, status.line + 1))
        case '\"' =>
          xs.findMatching('\"') match
            case Some(p) =>
              scanTokens(updateState(p, Some(TokenType.STRING), status.line))
            case None => Left(status.start, status.line)
        case c @ Digit() =>
          xs.findDigit match
            case Some(d) =>
              scanTokens(updateState(d, Some(TokenType.NUMBER), status.line))
            case None => Left(status.start, status.line)
        case c @ Letter() =>
          xs.findIdentifier match
            case Some(i) =>
              scanTokens(updateState(i, Some(TokenType.IDENTIFIER), status.line))
            case None => Left(status.start, status.line)
        case _ => Left(status.start, status.line)
      case Nil =>
        Left(status.start, status.line)
