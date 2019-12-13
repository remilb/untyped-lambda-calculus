package remilb.untypedlambdacalculus

import org.scalatest.fixture.FunSuite
import org.scalatest.{Matchers, Outcome}

class ParserTest extends FunSuite with Matchers {
  override type FixtureParam = Parser

  def withFixture(test: OneArgTest): Outcome = {
    val parser = new Parser
    withFixture(test.toNoArgTest(parser))
  }

  def getParseResult(parser: Parser, input: String): LambdaExpression = {
    parser.parseAll(parser.expr, input) match {
      case parser.Success(result, _) => result
      case parser.Failure(msg, remainingInput) =>
        fail(
          s"Failed to parse ${input}. Error msg: ${msg}. Remaining input: ${remainingInput}"
        )
    }
  }

  test("testParseAll") { parser =>
    val input = """(\x -> \y -> x y) (\g -> g)"""

    val expectedParse = Application(
      Abstraction(
        Var('x'),
        Abstraction(Var('y'), Application(Var('x'), Var('y')))
      ),
      Abstraction(Var('g'), Var('g'))
    )

    assertResult(expectedParse) {
      getParseResult(parser, input)
    }
  }

  test("Abstraction bodies should extend as far right as possible") { parser =>
    // Case 1
    val input = """\x -> x"""
    val expectedParseTree = Abstraction(Var('x'), Var('x'))
    getParseResult(parser, input) shouldBe expectedParseTree

    // Case 2
    val input2 = """\x -> x y"""
    val expectedParseTree2 =
      Abstraction(Var('x'), Application(Var('x'), Var('y')))
    getParseResult(parser, input2) shouldBe expectedParseTree2

    // Case 3
    val input3 = """\x -> b \y -> y x"""
    val expectedParseTree3 =
      Abstraction(
        Var('x'),
        Application(
          Var('b'),
          Abstraction(Var('y'), Application(Var('y'), Var('x')))
        )
      )
    getParseResult(parser, input3) shouldBe expectedParseTree3
  }

  test("Application without disambiguation should be left associative") {
    parser =>
      // Case 1
      var input = """f g h"""
      var expectedParseTree =
        Application(Application(Var('f'), Var('g')), Var('h'))
      getParseResult(parser, input) shouldBe expectedParseTree

      // Case 2
      input = """(\g -> g) (\f -> f) (\h -> h)"""
      expectedParseTree = Application(
        Application(
          Abstraction(Var('g'), Var('g')),
          Abstraction(Var('f'), Var('f'))
        ),
        Abstraction(Var('h'), Var('h'))
      )
      assertResult(expectedParseTree) {
        getParseResult(parser, input)
      }
  }

  test("Can change associativity with parentheses") { parser =>
    // Case 1
    var input = """f (g h)"""
    var expectedParseTree =
      Application(Var('f'), Application(Var('g'), Var('h')))
    getParseResult(parser, input) shouldBe expectedParseTree

    // Case 2
    input = """(\g -> g) ((\f -> f) (\h -> h))"""
    expectedParseTree = Application(
      Abstraction(Var('g'), Var('g')),
      Application(
        Abstraction(Var('f'), Var('f')),
        Abstraction(Var('h'), Var('h'))
      ),
    )
  }

  test("Nested abstractions should work") { parser =>
    var input = """(\h -> h \f -> f) \v -> v"""
    var expectedParseTree = Application(
      Abstraction(
        Var('h'),
        Application(Var('h'), Abstraction(Var('f'), Var('f')))
      ),
      Abstraction(Var('v'), Var('v'))
    )
    getParseResult(parser, input) shouldBe expectedParseTree

  }

  test("Redundant parentheses shouldn't matter") { parser =>
    // Case 1
    var input = """(((f)))"""
    var expectedParseTree: LambdaExpression = Var('f')
    getParseResult(parser, input) shouldBe expectedParseTree

    // Case 2
    input = """((((\f -> f))))"""
    expectedParseTree = Abstraction(Var('f'), Var('f'))
    getParseResult(parser, input) shouldBe expectedParseTree

    // Case 3
    input = """((((f f))))"""
    expectedParseTree = Application(Var('f'), Var('f'))
    getParseResult(parser, input) shouldBe expectedParseTree
  }

}
