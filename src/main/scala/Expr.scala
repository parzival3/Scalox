package github.parzival3.lox

sealed trait Expr
case class Unary(token: Token, expr: Expr) extends Expr
case class Binary(left: Expr, token: Token, right: Expr) extends Expr
case class Grouping(expr: Expr) extends Expr
case class Literal(value: AnyVal) extends Expr

def prettyPrint(expression: Expr): String =
  expression match
    case Binary(l, t, r) => s"(${t.lexeme} ${prettyPrint(l)}  ${prettyPrint(r)})"
    case Unary(t, e) => s"(${t.lexeme} ${prettyPrint(e)})"
    case Grouping(e) => s"(group ${prettyPrint(e)})"
    case Literal(v) => s"$v"

// @main def printAST(): Unit =
//     val expression: Expr = Binary(Unary(Token(TokenType.MINUS, "-", null, 1), Literal(123)), Token(TokenType.STAR, "*", null, 1), Grouping(Literal(45.67)))
//     println(prettyPrint(expression))
