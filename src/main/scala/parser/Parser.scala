package parser

import lexer.{ExtendsToken, AbstractClassToken, ClassToken, CloseCurlyBracketToken, ElementValueToken, EnumToken, ImplementsToken, InterfaceToken, OpenCurlyBracketToken, PackageToken, StartToken, Token}
import node.{AbstractClassNode, ClassNode, EnumNode, InterfaceNode, Node, Nodes, PackageNode}

import scala.annotation.tailrec

object Parser {
  def parse(tokens: List[_ <: Token], scopeNodes: Nodes = Nodes(), allNodes: Nodes = Nodes()): Nodes = {
    tokens match {
      case StartToken(_) :: rest => parse(rest, scopeNodes, allNodes)
      case PackageToken(_) :: ElementValueToken(name) :: OpenCurlyBracketToken(_) :: rest =>
        val (insidePackageTokens, remainingTokens) = spanScope(rest, 1)
        val packageNode = PackageNode(name, parse(insidePackageTokens, Nodes(), allNodes))
        parse(remainingTokens, scopeNodes.add(packageNode), allNodes.add(packageNode))
      case ClassToken(_) :: rest => parseClassNode(rest, scopeNodes, allNodes)
      case AbstractClassToken(_) :: rest => parseAbstractClassNode(rest, scopeNodes, allNodes)
      case InterfaceToken(_) :: rest => parseNode[InterfaceNode](rest, scopeNodes, allNodes, InterfaceNode.apply(_))
      case EnumToken(_) :: rest => parseNode[EnumNode](rest, scopeNodes, allNodes, EnumNode.apply)
      case _ => scopeNodes
    }
  }

  private def parseClassNode(tokens: List[Token], scopeNodes: Nodes, allNodes: Nodes): Nodes = {
    tokens match {
      case ElementValueToken(name) :: ExtendsToken(_) :: ElementValueToken(parentName) :: rest =>
        val node = ClassNode(name)
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
        val node = ClassNode(name)
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
      case rest => parseNode[ClassNode](rest, scopeNodes, allNodes, ClassNode.apply(_))
    }
  }

  private def parseAbstractClassNode(tokens: List[Token], scopeNodes: Nodes, allNodes: Nodes): Nodes = {
    tokens match {
      case ElementValueToken(name) :: ExtendsToken(_) :: ElementValueToken(parentName) :: rest =>
        val node = AbstractClassNode(name)
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
        val node = AbstractClassNode(name)
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
      case rest => parseNode[AbstractClassNode](rest, scopeNodes, allNodes, AbstractClassNode.apply(_))
    }
  }

  private def parseNode[T <: Node](tokens: List[Token], scopeNodes: Nodes, allNodes: Nodes, nodeMaker: String => T): Nodes = {
    tokens match {
      case ElementValueToken(name) :: OpenCurlyBracketToken(_) :: rest =>
        val (_, remainingTokens) = spanScope(rest, 1)
        parse(remainingTokens, scopeNodes.add(nodeMaker(name)), allNodes.add(nodeMaker(name)))
      case ElementValueToken(name) :: rest =>
        parse(rest, scopeNodes.add(nodeMaker(name)), allNodes.add(nodeMaker(name)))
      case _ => scopeNodes
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
