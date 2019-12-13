package remilb.untypedlambdacalculus

import scala.io.StdIn.readLine

class LambdaCalculusREPL(parser: Parser) {
  // Greek lambda for prompt
  val prompt = "\u03BB> "

  def run(): Unit = {
    var input = ""
    println("Enter lambda expression to evaluate or enter :q to quit")
    do {
      print(prompt)
      input = readLine()
      val parseResult = parser.parseAll(parser.expr, input)
      parseResult match {
        //case parseResult.msg == "'(' expected but ':' found" => println("Bye!")
        case parser.Success(result, _) =>
          println(evaluateLambdaExpression(result))
        case parser.NoSuccess("`(' expected but `:' found", _) =>
          println("Bye!")
        case parser.NoSuccess(_, _) => println("Not a valid lambda term!")
      }
    } while (input != ":q")
  }
}

object LambdaCalculusREPL {
  def main(args: Array[String]): Unit = {
    val repl = new LambdaCalculusREPL(new Parser)
    repl.run()
  }
}
