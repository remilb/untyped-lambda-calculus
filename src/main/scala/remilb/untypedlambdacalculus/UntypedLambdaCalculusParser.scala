package remilb.untypedlambdacalculus

import scala.util.parsing.combinator.{JavaTokenParsers, PackratParsers}

/**
 * Parser Combinator class for parsing remilb.untypedlambdacalculus.Lambda expression strings into ASTs of remilb.untypedlambdacalculus.Lambda Expressions
 * Grammar implemented more or less as per Pierce 5-3, with addition of parentheses for diambiguation. Applications
 * are left binding. Chapter 33 of Scala book on Parser Combinators used to figure out implementation. Use of
 * PackratParsers trait prevents blowing the stack through infinite left recursion, discovered through tip on StackOverflow
 */
class UntypedLambdaCalculusParser extends JavaTokenParsers with PackratParsers{
  lazy val variable: PackratParser[Var] = """[a-z]""".r ^^ {x => Var(x.charAt(0))}
  lazy val abstraction: PackratParser[Lambda] = "\\"~>variable~"."~term ^^ {case boundVar~"."~body => Lambda(boundVar, body)}
  lazy val application: PackratParser[Apply] = term~(abstraction | variable | term) ^^ {case term1~term2 => Apply(term1, term2)}
  lazy val parensTerm: PackratParser[LambdaExpression] = "(" ~> term <~ ")" ^^ {term => term}
  lazy val term: PackratParser[LambdaExpression] =  abstraction | application | variable | parensTerm
}