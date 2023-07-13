package parser

import lexer.{AbstractClassToken, ClassToken, CloseCurlyBracketToken, ElementValueToken, EnumToken, InterfaceToken, OpenCurlyBracketToken, PackageToken, StartToken, Token}
import node.{AbstractClassNode, ClassNode, EnumNode, InterfaceNode, Node, Nodes, PackageNode}

import scala.annotation.tailrec

object Parser {
  def parse(tokens: List[_ <: Token], nodes: Nodes = Nodes()): Nodes = {
    tokens match {
      case StartToken(_) :: rest => parse(rest, nodes)
      case PackageToken(_) :: ElementValueToken(name) :: OpenCurlyBracketToken(_) :: rest =>
        val (insidePackageTokens, remainingTokens) = spanScope(rest, 1)
        val packageNode = PackageNode(name, parse(insidePackageTokens, Nodes()))
        parse(remainingTokens, nodes.add(packageNode))
      case ClassToken(_) :: rest => parseNode[ClassNode](rest, nodes, ClassNode.apply(_))
      case AbstractClassToken(_) :: rest => parseNode[AbstractClassNode](rest, nodes, AbstractClassNode.apply(_))
      case InterfaceToken(_) :: rest => parseNode[InterfaceNode](rest, nodes, InterfaceNode.apply(_))
      case EnumToken(_) :: rest => parseNode[EnumNode](rest, nodes, EnumNode.apply)
      case _ => nodes
    }
  }

  private def parseNode[T <: Node](tokens: List[Token], nodes: Nodes, nodeMaker: String => T): Nodes = {
    tokens match {
      case ElementValueToken(name) :: OpenCurlyBracketToken(_) :: rest =>
        val (insideTokens, remainingTokens) = spanScope(rest, 1)
        // TODO: Currently we are not using insideTokens as we are not yet handling the parsing inside the scope.
        parse(remainingTokens, nodes.add(nodeMaker(name)))
      case ElementValueToken(name) :: rest =>
        parse(rest, nodes.add(nodeMaker(name)))
      case _ => nodes
    }
  }
  @tailrec
  private def spanScope(tokens: List[Token], level: Int, acc: List[Token] = List()): (List[Token], List[Token]) = tokens match {
    case OpenCurlyBracketToken(_) :: rest => spanScope(rest, level + 1, OpenCurlyBracketToken("{") :: acc)
    case CloseCurlyBracketToken(_) :: rest if level > 1 => spanScope(rest, level - 1, CloseCurlyBracketToken("}") :: acc)
    case CloseCurlyBracketToken(_) :: rest => ((acc).reverse, rest)
    case token :: rest => spanScope(rest, level, token :: acc)
    case Nil => throw new Exception("Unmatched open bracket")
  }
}
