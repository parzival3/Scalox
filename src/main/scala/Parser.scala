package github.parzival3.lox

// case class ParserState(tokens: List[Token], expr: Option[Expr])

// class Parser(tokens: List[Token]):

//     def matchAndApply(state: ParserState, matchingTokens: List[Token], func: (ParserState) => ParserState): ParserState =
//       if matchingTokens.exist(state.tokens.head) then
//           val newState = func(state)
//           val newExpr = Expr(state.expr, state.tokens.head, newState.comparsion)
//           matchAndApply(ParserStet(state.tokens.tails, newExpr), matchingTokens, func)
//       else
//         state


//     def expression(status: ParserStatus): ParserStatus =
//       equality(status)


//     def equality(status: ParserStatus): ParserStatus =
//       val status = comparsion(ctokens)
//       val grammarTokens = List[Token](TokenType.EQUAL_EQUAL, TokenType.BANG_EQUAL)
//       matchAndApply(status, grammarTokens, comparsion)

//     def comparsion(ctokens: List[Token]])): Expr =
//     def term(ctokens: List[Token]): Expr =
//     def factor(ctokens: List[Token]): Expr =
//     def unary(ctokens: List[Token]): Expr =
//     def primary(ctokens: List[Token]): Expr =


//     def parse: Expr =
//       equality()
