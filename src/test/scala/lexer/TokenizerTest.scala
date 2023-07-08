package lexer

import org.scalatest.funsuite.AnyFunSuite

class TokenizerTest extends AnyFunSuite:
  test("lexer.Tokenizer.tokenize") {
    val input =
      """@startuml
        |  ClassA --|> ClassB
        |  ClassB..|>ClassC
        |  ClassD<--ClassC
        |@enduml""".stripMargin

    val tokens = Tokenizer.tokenize(input)
    val expectedTokens = List(
      StartToken("@startuml"),
      ElementValueToken("ClassA"),
      RightArrowToken("--|>"),
      ElementValueToken("ClassB"),
      ElementValueToken("ClassB"),
      RightArrowToken("..|>"),
      ElementValueToken("ClassC"),
      ElementValueToken("ClassD"),
      LeftArrowToken("<--"),
      ElementValueToken("ClassC"),
      EndToken("@enduml")
    )
    assert(tokens == expectedTokens)
  }