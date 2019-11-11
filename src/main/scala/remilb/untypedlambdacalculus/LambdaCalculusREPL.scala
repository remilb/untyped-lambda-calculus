package remilb.untypedlambdacalculus

import scala.io.StdIn.readLine


object LambdaCalculusREPL {
  def main(args: Array[String]): Unit = {
    val parser = new UntypedLambdaCalculusParser
    var input = ""
    println("Enter lambda expression to evaluate or enter :q to quit")
    do {
      print(s"${0x03BB.toChar}> ")
      input = readLine()
      val parseResult = parser.parseAll(parser.term, input)
      parseResult match {
        //case parseResult.msg == "'(' expected but ':' found" => println("Bye!")
        case parser.Success(result, _) => println(evaluateLambdaExpression(result))
        case parser.NoSuccess("`(' expected but `:' found", _) => println("Bye!")
        case parser.NoSuccess(_, _) => println("Not a valid lambda term!")
      }
    } while (input != ":q")
  }
}
