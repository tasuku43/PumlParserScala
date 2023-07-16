package node

sealed trait Node {
  def name: String

  def accept(nodeVisitor: NodeVisitor): Node
}

sealed trait ClassLikeNode extends Node {
  def extend(parent: ClassLikeNode): ClassLikeNode

  def implement(interface: InterfaceNode): ClassLikeNode

  def accept(nodeVisitor: NodeVisitor): ClassLikeNode
}

case class Nodes(nodes: Seq[Node] = Seq.empty) {
  def add(node: Node): Nodes = Nodes(nodes :+ node)

  def accept(NodeVisitor: NodeVisitor): Nodes = Nodes(nodes.map(_.accept(NodeVisitor)))

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

  def accept(nodeVisitor: NodeVisitor): PackageNode =
    this.copy(children = children.accept(nodeVisitor))
}

case class InterfaceNode(
  name: String,
  parent: Option[InterfaceNode] = None,
) extends Node {
  def extend(parent: InterfaceNode): InterfaceNode = this.copy(parent = Some(parent))

  def accept(nodeVisitor: NodeVisitor): InterfaceNode =
    nodeVisitor.visit[InterfaceNode](this).copy(parent = parent.map(_.accept(nodeVisitor)))
}

case class ClassNode(
  name: String,
  parent: Option[ClassLikeNode] = None,
  interfaces: Seq[InterfaceNode] = Seq.empty
) extends ClassLikeNode {
  def extend(parent: ClassLikeNode): ClassNode = this.copy(parent = Some(parent))

  def implement(interface: InterfaceNode): ClassNode = this.copy(interfaces = this.interfaces :+ interface)

  def accept(nodeVisitor: NodeVisitor): ClassNode = {
    val accepted = nodeVisitor.visit[ClassNode](this)
    accepted.copy(
      parent = accepted.parent.map(_.accept(nodeVisitor)),
      interfaces = accepted.interfaces.map(_.accept(nodeVisitor))
    )
  }
}

case class AbstractClassNode(
  name: String,
  parent: Option[ClassLikeNode] = None,
  interfaces: Seq[InterfaceNode] = Seq.empty
) extends ClassLikeNode {
  def extend(parent: ClassLikeNode): AbstractClassNode = this.copy(parent = Some(parent))

  def implement(interface: InterfaceNode): AbstractClassNode = this.copy(interfaces = this.interfaces :+ interface)

  def accept(nodeVisitor: NodeVisitor): AbstractClassNode = {
    val accepted = nodeVisitor.visit[AbstractClassNode](this)
    accepted.copy(
      parent = accepted.parent.map(_.accept(nodeVisitor)),
      interfaces = accepted.interfaces.map(_.accept(nodeVisitor))
    )
  }
}

case class EnumNode(
  name: String,
) extends Node {
  def accept(nodeVisitor: NodeVisitor): EnumNode =
    nodeVisitor.visit[EnumNode](this)
}

trait NodeVisitor {
  def visit[T <: Node](node: T): T
}

class NodeReplacer(val target: Node, val replacement: Node) extends NodeVisitor {
  def visit[T <: Node](node: T): T = {
    if (node == target) {
      replacement.asInstanceOf[T]
    } else {
      node
    }
  }
}