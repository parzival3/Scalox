package github.parzival3.lox

case class ParserState(tokens: List[Token], expr: Option[Expr])

class Parser(tokens: List[Token]):

    def matchAndApply(state: ParserState, matchingTokens: List[TokenType], func: (ParserState) => ParserState): ParserState =
      if matchingTokens.exists(x => x == state.tokens.head.tokenType) then
          val newState = func(state)
          val newExpr = Binary(state.expr.get, state.tokens.head, newState.expr.get)
          matchAndApply(ParserState(state.tokens.tail, Some(newExpr)), matchingTokens, func)
      else
        state


    def expression(state: ParserState): ParserState =
      equality(state)


    def equality(state: ParserState): ParserState =
      val cState = comparsion(state)
      val grammarTokens = List[TokenType](TokenType.EQUAL_EQUAL, TokenType.BANG_EQUAL)
      matchAndApply(cState, grammarTokens, comparsion)

    def comparsion(state: ParserState): ParserState =
      val cState = term(state)
      val grammarTokens = List[TokenType](TokenType.GREATER, TokenType.GREATER_EQUAL, TokenType.LESS, TokenType.LESS_EQUAL)
      matchAndApply(cState, grammarTokens, term)

    def term(state: ParserState): ParserState =
      val cState = factor(state)
      val grammarTokens = List[TokenType](TokenType.PLUS, TokenType.MINUS)
      matchAndApply(cState, grammarTokens, factor)

    def factor(state: ParserState): ParserState =
      val cState = unary(state)
      val grammarTokens = List[TokenType](TokenType.STAR, TokenType.SLASH)
      matchAndApply(cState, grammarTokens, unary)

    def unary(state: ParserState): ParserState =
      val matchingTokens = List[TokenType](TokenType.BANG, TokenType.MINUS)

      if matchingTokens.exists(x => x == state.tokens.head.tokenType) then
        val newState = state.copy(tokens = state.tokens.tail)
        val stat = unary(newState)
        val newExpr = Unary(state.tokens.head, stat.expr.get)
        ParserState(stat.tokens, Some(newExpr))
      else
        primary(state)

    def primary(state: ParserState): ParserState =
      val matchingTokens = List[TokenType](TokenType.NUMBER)
      if matchingTokens.exists(x => x == state.tokens.head.tokenType) then
         val expr = Literal(state.tokens.head.literal.get)
         ParserState(state.tokens.tail, Some(expr))
      else
        state.copy(expr = None)


    def parse: Expr =
      unary(ParserState(tokens, None)).expr.get
