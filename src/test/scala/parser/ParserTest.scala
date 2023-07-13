package parser

import node.{AbstractClassNode, ClassNode, EnumNode, InterfaceNode, Nodes, PackageNode}
import lexer.Tokenizer
import org.scalatest.funsuite.AnyFunSuite

class ParserTest extends AnyFunSuite:
  test("Tokenization of a simple PlantUML diagram") {
    val input =
      """@startuml
        |package Lexer {
        |    interface Tokenizeable
        |    package Lexer/Arrow {
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
    val ast = Parser.parse(tokens, Nodes())
    val expected = Nodes(Seq(
      PackageNode("Lexer", Nodes(Seq(
        InterfaceNode("Tokenizeable"),
        PackageNode("Lexer/Arrow", Nodes(Seq(
          ClassNode("LeftArrowTokenizer")
        ))),
        PackageNode("Lexer/CurlyBracket", Nodes(Seq(
          AbstractClassNode("CurlyBracketTokenizer"),
          ClassNode("OpenCurlyBracketToken")
        ))),
        EnumNode("Enum"),
      ))),
    ))
    assert(ast == expected)
  }

