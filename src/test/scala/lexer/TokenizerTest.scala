package lexer

import org.scalatest.funsuite.AnyFunSuite

class TokenizerTest extends AnyFunSuite:
  test("Tokenization of a simple PlantUML diagram") {
    val input =
      """@startuml
        |class SomeClass {
        |    + publicProperty
        |    # protectedProperty
        |    - privateProperty
        |}
        |abstract class SomeAbstractClass
        |interface SomeInterface
        |
        |SomeAbstractClass<|-- SomeClass
        |SomeAbstractClass..|> SomeInterface
        |
        |package SomePackage {
        |    interface SomeInterface2
        |    abstract class SomeAbstractClass2 implements SomeInterface2
        |    class SomeClass2 extends SomeAbstractClass2
        |    enum SomeEnum {
        |      CASE1
        |      CASE2
        |      CASE3
        |    }
        |}
        |@enduml""".stripMargin

    val tokens = Tokenizer.tokenize(input)
    val expectedTokens = List(
      StartToken("@startuml"),
      ClassToken("class"),
      ElementValueToken("SomeClass"),
      OpenCurlyBracketToken("{"),
      PublicVisibilityToken("+"),
      ElementValueToken("publicProperty"),
      ProtectedVisibilityToken("#"),
      ElementValueToken("protectedProperty"),
      PrivateVisibilityToken("-"),
      ElementValueToken("privateProperty"),
      CloseCurlyBracketToken("}"),
      AbstractClassToken("abstract class"),
      ElementValueToken("SomeAbstractClass"),
      InterfaceToken("interface"),
      ElementValueToken("SomeInterface"),
      ElementValueToken("SomeAbstractClass"),
      LeftArrowToken("<|--"),
      ElementValueToken("SomeClass"),
      ElementValueToken("SomeAbstractClass"),
      RightArrowToken("..|>"),
      ElementValueToken("SomeInterface"),
      PackageToken("package"),
      ElementValueToken("SomePackage"),
      OpenCurlyBracketToken("{"),
      InterfaceToken("interface"),
      ElementValueToken("SomeInterface2"),
      AbstractClassToken("abstract class"),
      ElementValueToken("SomeAbstractClass2"),
      ImplementsToken("implements"),
      ElementValueToken("SomeInterface2"),
      ClassToken("class"),
      ElementValueToken("SomeClass2"),
      ExtendsToken("extends"),
      ElementValueToken("SomeAbstractClass2"),
      EnumToken("enum"),
      ElementValueToken("SomeEnum"),
      OpenCurlyBracketToken("{"),
      ElementValueToken("CASE1"),
      ElementValueToken("CASE2"),
      ElementValueToken("CASE3"),
      CloseCurlyBracketToken("}"),
      CloseCurlyBracketToken("}"),
      EndToken("@enduml")
    )
    assert(tokens == expectedTokens)
  }

  test("Tokenization of a complex PlantUML diagram with various elements") {
    val input =
      """@startuml
        |package Lexer {
        |    interface Tokenizeable
        |    package Lexer/Arrow {
        |        abstract class ArrowTokenizer implements Tokenizeable
        |        class LeftArrowTokenizer extends ArrowTokenizer {
        |            + publicProperty : array
        |            # protectedProperty
        |            - privateProperty : string
        |        }
        |    }
        |    package Lexer/CurlyBracket {
        |        abstract class CurlyBracketTokenizer
        |        class OpenCurlyBracketToken
        |
        |        CurlyBracketTokenizer..|>Tokenizeable
        |        CurlyBracketTokenizer<|--OpenCurlyBracketToken
        |    }
        |    enum Enum {
        |      CASE1
        |      CASE2
        |      CASE3
        |    }
        |
        |    NoneDefinitionClass ..|> Tokenizeable
        |}
        |@enduml""".stripMargin

    val tokens = Tokenizer.tokenize(input)
    val expectedTokens = List(
      StartToken("@startuml"),
      PackageToken("package"),
      ElementValueToken("Lexer"),
      OpenCurlyBracketToken("{"),
      InterfaceToken("interface"),
      ElementValueToken("Tokenizeable"),
      PackageToken("package"),
      ElementValueToken("Lexer/Arrow"),
      OpenCurlyBracketToken("{"),
      AbstractClassToken("abstract class"),
      ElementValueToken("ArrowTokenizer"),
      ImplementsToken("implements"),
      ElementValueToken("Tokenizeable"),
      ClassToken("class"),
      ElementValueToken("LeftArrowTokenizer"),
      ExtendsToken("extends"),
      ElementValueToken("ArrowTokenizer"),
      OpenCurlyBracketToken("{"),
      PublicVisibilityToken("+"),
      ElementValueToken("publicProperty"),
      ColonToken(":"),
      ElementValueToken("array"),
      ProtectedVisibilityToken("#"),
      ElementValueToken("protectedProperty"),
      PrivateVisibilityToken("-"),
      ElementValueToken("privateProperty"),
      ColonToken(":"),
      ElementValueToken("string"),
      CloseCurlyBracketToken("}"),
      CloseCurlyBracketToken("}"),
      PackageToken("package"),
      ElementValueToken("Lexer/CurlyBracket"),
      OpenCurlyBracketToken("{"),
      AbstractClassToken("abstract class"),
      ElementValueToken("CurlyBracketTokenizer"),
      ClassToken("class"),
      ElementValueToken("OpenCurlyBracketToken"),
      ElementValueToken("CurlyBracketTokenizer"),
      RightArrowToken("..|>"),
      ElementValueToken("Tokenizeable"),
      ElementValueToken("CurlyBracketTokenizer"),
      LeftArrowToken("<|--"),
      ElementValueToken("OpenCurlyBracketToken"),
      CloseCurlyBracketToken("}"),
      EnumToken("enum"),
      ElementValueToken("Enum"),
      OpenCurlyBracketToken("{"),
      ElementValueToken("CASE1"),
      ElementValueToken("CASE2"),
      ElementValueToken("CASE3"),
      CloseCurlyBracketToken("}"),
      ElementValueToken("NoneDefinitionClass"),
      RightArrowToken("..|>"),
      ElementValueToken("Tokenizeable"),
      CloseCurlyBracketToken("}"),
      EndToken("@enduml")
    )
    assert(tokens == expectedTokens)
  }
