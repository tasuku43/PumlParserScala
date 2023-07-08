package lexer

import org.scalatest.funsuite.AnyFunSuite

class TokenizerTest extends AnyFunSuite:
  test("lexer.Tokenizer.tokenize") {
    val input =
      """@startuml
        |@enduml""".stripMargin

    val tokens = Tokenizer.tokenize(input)
    val expectedTokens = List(
      StartToken("@startuml"),
      EndToken("@enduml")
    )
    assert(tokens == expectedTokens)
  }