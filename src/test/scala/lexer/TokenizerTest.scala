package lexer

import org.scalatest.funsuite.AnyFunSuite

class TokenizerTest extends AnyFunSuite:
  test("lexer.Tokenizer.tokenize correctly tokenizes complex PlantUML diagram") {
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
