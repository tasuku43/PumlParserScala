package lexer

import scala.annotation.tailrec
import scala.util.matching.Regex

trait TokenGenerator[T <: Token] {
  def createToken(input: String): Option[T]
}

class DirectMatchTokenGenerator[T <: Token](val pattern: Regex, val create: String => T) extends TokenGenerator[T] {
  def createToken(input: String): Option[T] = {
    pattern.findFirstIn(input).map(create)
  }
}

class UpToPatternTokenGenerator[T <: Token](val tokenEndPatterns: Set[Regex], val create: String => T) extends TokenGenerator[T] {
  def createToken(input: String): Option[T] = {
    @tailrec
    def findMatch(input: String, collected: String = ""): Option[String] = {
      if (input.isEmpty || tokenEndPatterns.exists(_.findFirstIn(input).isDefined)) {
        Option(collected).filter(_.nonEmpty)
      } else {
        findMatch(input.tail, collected + input.head)
      }
    }

    findMatch(input).map(create)
  }
}

class TokenGenerators(generators: List[TokenGenerator[_ <: Token]]) {
  def createToken(input: String): Option[_ <: Token] = {
    generators.flatMap(_.createToken(input)).headOption
  }
}

object TokenGenerators {
  def factory(): TokenGenerators = new TokenGenerators(List(
    new DirectMatchTokenGenerator[StartToken](StartToken.regex, StartToken.apply),
    new DirectMatchTokenGenerator[LeftArrowToken](LeftArrowToken.regex, LeftArrowToken.apply),
    new DirectMatchTokenGenerator[RightArrowToken](RightArrowToken.regex, RightArrowToken.apply),
    new DirectMatchTokenGenerator[EndToken](EndToken.regex, EndToken.apply),
    new DirectMatchTokenGenerator[PackageToken](PackageToken.regex, PackageToken.apply),
    new DirectMatchTokenGenerator[ClassToken](ClassToken.regex, ClassToken.apply),
    new DirectMatchTokenGenerator[AbstractClassToken](AbstractClassToken.regex, AbstractClassToken.apply),
    new DirectMatchTokenGenerator[AbstractToken](AbstractToken.regex, AbstractToken.apply),
    new DirectMatchTokenGenerator[InterfaceToken](InterfaceToken.regex, InterfaceToken.apply),
    new DirectMatchTokenGenerator[EnumToken](EnumToken.regex, EnumToken.apply),
    new DirectMatchTokenGenerator[NamespaceToken](NamespaceToken.regex, NamespaceToken.apply),
    new DirectMatchTokenGenerator[OpenCurlyBracketToken](OpenCurlyBracketToken.regex, OpenCurlyBracketToken.apply),
    new DirectMatchTokenGenerator[CloseCurlyBracketToken](CloseCurlyBracketToken.regex, CloseCurlyBracketToken.apply),
    new DirectMatchTokenGenerator[OpenRoundBracketToken](OpenRoundBracketToken.regex, OpenRoundBracketToken.apply),
    new DirectMatchTokenGenerator[CloseRoundBracketToken](CloseRoundBracketToken.regex, CloseRoundBracketToken.apply),
    new DirectMatchTokenGenerator[ExtendsToken](ExtendsToken.regex, ExtendsToken.apply),
    new DirectMatchTokenGenerator[ImplementsToken](ImplementsToken.regex, ImplementsToken.apply),
    new DirectMatchTokenGenerator[PrivateVisibilityToken](PrivateVisibilityToken.regex, PrivateVisibilityToken.apply),
    new DirectMatchTokenGenerator[PublicVisibilityToken](PublicVisibilityToken.regex, PublicVisibilityToken.apply),
    new DirectMatchTokenGenerator[ProtectedVisibilityToken](ProtectedVisibilityToken.regex, ProtectedVisibilityToken.apply),
    new UpToPatternTokenGenerator[ElementValueToken](ElementValueToken.tokenEndPatterns, ElementValueToken.apply)
  ))
}
