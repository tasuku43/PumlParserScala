package node

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class NodeTest extends AnyFunSuite:
  test("Nodes#findFirst should find the first node of a specific type with the specified name") {
    val nodes = Nodes(Seq(
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

    nodes.findFirst[ClassNode]("LeftArrowTokenizer") should be(Some(ClassNode("LeftArrowTokenizer")))
    nodes.findFirst[InterfaceNode]("Tokenizeable") should be(Some(InterfaceNode("Tokenizeable")))
    nodes.findFirst[AbstractClassNode]("CurlyBracketTokenizer") should be(Some(AbstractClassNode("CurlyBracketTokenizer")))
    nodes.findFirst[EnumNode]("Enum") should be(Some(EnumNode("Enum")))

    nodes.findFirst[ClassNode]("NonexistentNode") should be(None)
    nodes.findFirst[InterfaceNode]("NonexistentNode") should be(None)
    nodes.findFirst[AbstractClassNode]("NonexistentNode") should be(None)
    nodes.findFirst[EnumNode]("NonexistentNode") should be(None)
  }