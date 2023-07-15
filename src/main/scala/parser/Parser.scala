package parser

import lexer.{AbstractClassToken, ClassToken, CloseCurlyBracketToken, ElementValueToken, EnumToken, ExtendsToken, ImplementsToken, InterfaceToken, OpenCurlyBracketToken, PackageToken, StartToken, Token}
import node.{Implementable, ClassExtendable, AbstractClassNode, ClassNode, EnumNode, InterfaceNode, Node, Nodes, PackageNode}

import scala.annotation.tailrec

object Parser {
  def parse(tokens: List[_ <: Token], scopeNodes: Nodes = Nodes(), allNodes: Nodes = Nodes()): Nodes = {
    tokens match {
      case StartToken(_) :: rest => parse(rest, scopeNodes, allNodes)
      case PackageToken(_) :: ElementValueToken(name) :: OpenCurlyBracketToken(_) :: rest =>
        val (insidePackageTokens, remainingTokens) = spanScope(rest, 1)
        val packageNode = PackageNode(name, parse(insidePackageTokens, Nodes(), allNodes))
        parse(remainingTokens, scopeNodes.add(packageNode), allNodes.add(packageNode))
      case ClassToken(_) :: rest => parseClassNode(rest, scopeNodes, allNodes, ClassNode.apply(_))
      case AbstractClassToken(_) :: rest => parseClassNode(rest, scopeNodes, allNodes, AbstractClassNode.apply(_))
      case InterfaceToken(_) :: rest => parseInterfaceNode(rest, scopeNodes, allNodes)
      case EnumToken(_) :: rest => parseEnumNode(rest, scopeNodes, allNodes)
      case _ => scopeNodes
    }
  }

  private def parseEnumNode(tokens: List[Token], scopeNodes: Nodes, allNodes: Nodes): Nodes = {
    tokens match {
      case ElementValueToken(name) :: OpenCurlyBracketToken(_) :: rest =>
        val (_, remainingTokens) = spanScope(rest, 1)
        parse(remainingTokens, scopeNodes.add(EnumNode(name)), allNodes.add(EnumNode(name)))
      case ElementValueToken(name) :: rest =>
        parse(rest, scopeNodes.add(EnumNode(name)), allNodes.add(EnumNode(name)))
      case _ => allNodes
    }
  }

  private def parseClassNode(
    tokens: List[Token], scopeNodes: Nodes, allNodes: Nodes, nodeMaker: String => ClassExtendable & Implementable & Node
  ): Nodes = {
    tokens match {
      case ElementValueToken(name) :: ExtendsToken(_) :: ElementValueToken(parentName) :: rest =>
        val node = nodeMaker(name)
        val parentNode = allNodes.findFirst[ClassNode | AbstractClassNode](parentName) match {
          case Some(parentNode) => parentNode
          case None => throw new Exception(s"Parent class $parentName not found")
        }
        rest match
          case ImplementsToken(_) :: ElementValueToken(interfaceName) :: rest => ???
          case OpenCurlyBracketToken(_) :: rest =>
            val (_, remainingTokens) = spanScope(rest, 1)
            parse(remainingTokens, scopeNodes.add(node.extend(parentNode)), allNodes.add(node.extend(parentNode)))
          case rest => parse(rest, scopeNodes.add(node.extend(parentNode)), allNodes.add(node.extend(parentNode)))
      case ElementValueToken(name) :: ImplementsToken(_) :: ElementValueToken(interfaceName) :: rest =>
        val node = nodeMaker(name)
        val interfaceNode = allNodes.findFirst[InterfaceNode](interfaceName) match {
          case Some(parentNode) => parentNode
          case None => throw new Exception(s"Interface $interfaceName not found")
        }
        rest match
          case ExtendsToken(_) :: ElementValueToken(interfaceName) :: rest => ???
          case OpenCurlyBracketToken(_) :: rest =>
            val (_, remainingTokens) = spanScope(rest, 1)
            parse(remainingTokens, scopeNodes.add(node.implement(interfaceNode)), allNodes.add(node.implement(interfaceNode)))
          case rest => parse(rest, scopeNodes.add(node.implement(interfaceNode)), allNodes.add(node.implement(interfaceNode)))
      case ElementValueToken(name) :: OpenCurlyBracketToken(_) :: rest =>
        val (_, remainingTokens) = spanScope(rest, 1)
        parse(remainingTokens, scopeNodes.add(nodeMaker(name)), allNodes.add(nodeMaker(name)))
      case ElementValueToken(name) :: rest =>
        parse(rest, scopeNodes.add(nodeMaker(name)), allNodes.add(nodeMaker(name)))
      case _ => allNodes
    }
  }

  private def parseInterfaceNode(
    tokens: List[Token], scopeNodes: Nodes, allNodes: Nodes
  ): Nodes = {
    tokens match {
      case ElementValueToken(name) :: ExtendsToken(_) :: ElementValueToken(parentName) :: rest =>
        val node = InterfaceNode(name)
        val parentNode = allNodes.findFirst[InterfaceNode](parentName) match {
          case Some(parentNode) => parentNode
          case None => throw new Exception(s"Parent class $parentName not found")
        }
        rest match
          case OpenCurlyBracketToken(_) :: rest =>
            val (_, remainingTokens) = spanScope(rest, 1)
            parse(remainingTokens, scopeNodes.add(node.extend(parentNode)), allNodes.add(node.extend(parentNode)))
          case rest => parse(rest, scopeNodes.add(node.extend(parentNode)), allNodes.add(node.extend(parentNode)))
      case ElementValueToken(name) :: OpenCurlyBracketToken(_) :: rest =>
        val (_, remainingTokens) = spanScope(rest, 1)
        parse(remainingTokens, scopeNodes.add(InterfaceNode(name)), allNodes.add(InterfaceNode(name)))
      case ElementValueToken(name) :: rest =>
        parse(rest, scopeNodes.add(InterfaceNode(name)), allNodes.add(InterfaceNode(name)))
      case _ => allNodes
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
