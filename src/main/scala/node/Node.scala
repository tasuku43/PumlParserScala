/*
 * This file defines the structure of an Abstract Syntax Tree (AST) for a programming language model.
 * The design of the classes and their relationships are intended to follow the behavior of Scala's classes, traits (interfaces), and enums.
 * Please note that while this model is inspired by Scala, it is a simplified representation and does not capture all aspects of Scala's type system.
 * Specifically, the current version of this model does not account for fields (properties) that can exist within Scala's classes, traits, and enums.
 */
package node

sealed trait Node {
  def name: String
}

case class Nodes(nodes: Seq[Node] = Seq.empty) {
  def add(node: Node): Nodes = Nodes(nodes :+ node)

  def findFirst[T <: Node](name: String)(implicit tag: reflect.ClassTag[T]): Option[T] = {
    nodes.flatMap {
      case t: T if t.name == name => Some(t)
      case p: PackageNode => p.children.findFirst[T](name)
      case _ => None
    }.headOption
  }
}

case class PackageNode(name: String, children: Nodes = Nodes()) extends Node {
  def add(node: Node): PackageNode = this.copy(children = children.add(node))
}

case class InterfaceNode(
  name: String,
  parent: Option[InterfaceNode] = None,
) extends Node {
  def extend(parent: InterfaceNode): InterfaceNode = this.copy(parent = Some(parent))
}

case class ClassNode(
  name: String,
  parent: Option[ClassNode | AbstractClassNode] = None,
  interfaces: Seq[InterfaceNode] = Seq.empty
) extends Node {
  def extend(parent: ClassNode | AbstractClassNode): ClassNode = this.copy(parent = Some(parent))

  def implement(interface: InterfaceNode): ClassNode = this.copy(interfaces = this.interfaces :+ interface)
}

case class AbstractClassNode(
  name: String,
  parent: Option[ClassNode | AbstractClassNode] = None,
  interfaces: Seq[InterfaceNode] = Seq.empty
) extends Node {
  def extend(parent: ClassNode | AbstractClassNode): AbstractClassNode = this.copy(parent = Some(parent))

  def implement(interface: InterfaceNode): AbstractClassNode = this.copy(interfaces = this.interfaces :+ interface)
}

case class EnumNode(
  name: String,
) extends Node {
}
