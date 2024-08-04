package github.parzival3.lox

case class ParserState(tokens: List[Token], expr: Option[Expr], error: Boolean = false)

class Parser(tokens: List[Token]):

    def matchAndApply(state: ParserState, matchingTokens: List[TokenType], func: (ParserState) => ParserState): ParserState =
      println("Hello")
      if state.tokens != Nil then
        if matchingTokens.exists(x => x == state.tokens.head.tokenType) then
            val newState = func(state.copy(tokens = state.tokens.tail))
            val newExpr = Binary(state.expr.get, state.tokens.head, newState.expr.get)

            if newState.tokens.isEmpty || newState.tokens.tails.isEmpty then
              ParserState(Nil, Some(newExpr))
            else
              matchAndApply(ParserState(newState.tokens.tail, Some(newExpr)), matchingTokens, func)

        else
          state
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

      val grammarTokens = List[TokenType](TokenType.PLUS, TokenType.MINUS)
      if state.expr.isDefined then
        matchAndApply(state, grammarTokens, factor)
      else
        val cState = factor(state)
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
         println(state.tokens.tail)
         ParserState(state.tokens.tail, Some(expr))
      else
        state.copy(expr = None, error = true)

    def parse: ParserState =
      val state = term(ParserState(tokens, None))
      state match
        case ParserState(Nil, Some(x), false) => state
        case ParserState(tokens, Some(x), false) =>
            println(state)
            term(state)
        case ParserState(tokens, _, true) => state
        case _ => state
