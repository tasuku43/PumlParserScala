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
        |}
        |@enduml""".stripMargin

    val tokens = Tokenizer.tokenize(input)
    val ast = Parser.parse(tokens, Nodes())
    val expected = Nodes(Seq(
      PackageNode("Lexer", Nodes(Seq(
        InterfaceNode("Tokenizeable"),
        PackageNode("Lexer/Arrow", Nodes(Seq(
          AbstractClassNode("ArrowTokenizer", parent = None, interfaces = Seq(
            InterfaceNode("Tokenizeable")
          )),
          ClassNode("LeftArrowTokenizer", Some(AbstractClassNode("ArrowTokenizer", interfaces = Seq(
            InterfaceNode("Tokenizeable")
          ))))
        ))),
        PackageNode("Lexer/CurlyBracket", Nodes(Seq(
          AbstractClassNode("CurlyBracketTokenizer", parent = None, interfaces = Seq(
            InterfaceNode("Tokenizeable")
          )),
          ClassNode("OpenCurlyBracketToken", parent = Some(AbstractClassNode("CurlyBracketTokenizer", parent = None, interfaces = Seq(
            InterfaceNode("Tokenizeable")
          ))))
        ))),
        EnumNode("Enum"),
      ))),
    ))
    assert(ast == expected)
  }
