package remilb.untypedlambdacalculus

import scala.util.parsing.combinator.{RegexParsers, PackratParsers}

/**
 * Parser Combinator class for parsing string representations of untyped lambda calculus expressions into ASTs of
 * LambdaExpressions. Grammar implemented more or less as per Pierce 5-3, with addition of parentheses for
 * disambiguation. Application is left associative.
 */
class UntypedLambdaCalculusParser extends RegexParsers with PackratParsers{
  // Lexer rules
  lazy val variable: PackratParser[Var] = """[a-z]""".r ^^ {x => Var(x.charAt(0))}
  lazy val abstraction: PackratParser[Lambda] = "\\"~>variable~"-"~term ^^ {case boundVar~"."~body => Lambda(boundVar, body)}
  lazy val application: PackratParser[Apply] = term~(abstraction | variable | term) ^^ {case term1~term2 => Apply(term1, term2)}
  lazy val parensTerm: PackratParser[LambdaExpression] = "(" ~> term <~ ")" ^^ {term => term}
  lazy val term: PackratParser[LambdaExpression] =  abstraction | application | variable | parensTerm
}