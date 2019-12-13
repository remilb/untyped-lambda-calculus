package remilb.untypedlambdacalculus

import scala.util.parsing.combinator.{RegexParsers, PackratParsers}

/**
  * Parser Combinator class for parsing string representations of untyped lambda calculus expressions into ASTs of
  * LambdaExpressions. Grammar implemented more or less as per Pierce 5-3, with addition of parentheses for
  * disambiguation. Application is left associative.
  */
class Parser extends RegexParsers with PackratParsers {
  // Lexer rules
  lazy val abstractionBegin: PackratParser[String] =
    """\\""".r ^^ {
      _.toString
    }

  lazy val abstractionBodyBegin: PackratParser[String] =
    """->""".r ^^ {
      _.toString
    }

  // Parser rules
  lazy val expr
    : PackratParser[LambdaExpression] = application | abstraction | variable | parensExpr

  lazy val parensExpr: PackratParser[LambdaExpression] = "(" ~> expr <~ ")"

  lazy val variable: PackratParser[Var] = """[a-zA-Z]""".r ^^ { x =>
    Var(x.charAt(0))
  }

  // TODO: Would be nice to make this type PackratParser[Abstraction]
  lazy val abstraction
    : PackratParser[LambdaExpression] = abstractionBegin ~> variable.* ~ (abstractionBodyBegin ~> expr) ^^ {
    case boundVars ~ body =>
      boundVars.foldRight(body)(Abstraction(_, _))
  }

  lazy val application
    : PackratParser[Application] = expr ~ (parensExpr | abstraction | variable) ^^ {
    case term1 ~ term2 => Application(term1, term2)
  }
}
