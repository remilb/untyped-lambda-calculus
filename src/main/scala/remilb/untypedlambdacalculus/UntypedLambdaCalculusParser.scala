package remilb.untypedlambdacalculus

import scala.util.parsing.combinator.{RegexParsers, PackratParsers}

/**
  * Parser Combinator class for parsing string representations of untyped lambda calculus expressions into ASTs of
  * LambdaExpressions. Grammar implemented more or less as per Pierce 5-3, with addition of parentheses for
  * disambiguation. Application is left associative.
  */
class UntypedLambdaCalculusParser extends RegexParsers with PackratParsers {
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
  lazy val variable: PackratParser[Var] = """[a-z]""".r ^^ { x =>
    Var(x.charAt(0))
  }

  lazy val abstraction
    : PackratParser[Abstraction] = abstractionBegin ~> variable ~ (abstractionBodyBegin ~> term) ^^ {
    case boundVar ~ body => Abstraction(boundVar, body)
  }

  lazy val application
    : PackratParser[Application] = term ~ (parensTerm | abstraction | variable) ^^ {
    case term1 ~ term2 => Application(term1, term2)
  }

  lazy val parensTerm: PackratParser[LambdaExpression] = "(" ~> term <~ ")" ^^ {
    term =>
      term
  }

  lazy val term
    : PackratParser[LambdaExpression] = application | abstraction | variable | parensTerm
}
