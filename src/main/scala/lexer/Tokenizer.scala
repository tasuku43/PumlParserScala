package lexer

import scala.annotation.tailrec
import scala.util.matching.Regex

abstract class Token(val value: String)

case class StartToken(override val value: String) extends Token(value)

case class EndToken(override val value: String) extends Token(value)

case class LeftArrowToken(override val value: String) extends Token(value)

case class RightArrowToken(override val value: String) extends Token(value)

case class PackageToken(override val value: String) extends Token(value)

case class ClassToken(override val value: String) extends Token(value)

case class AbstractClassToken(override val value: String) extends Token(value)

case class AbstractToken(override val value: String) extends Token(value)

case class InterfaceToken(override val value: String) extends Token(value)

case class EnumToken(override val value: String) extends Token(value)

case class NamespaceToken(override val value: String) extends Token(value)

case class OpenCurlyBracketToken(override val value: String) extends Token(value)

case class CloseCurlyBracketToken(override val value: String) extends Token(value)

case class OpenRoundBracketToken(override val value: String) extends Token(value)

case class CloseRoundBracketToken(override val value: String) extends Token(value)

case class ExtendsToken(override val value: String) extends Token(value)

case class ImplementsToken(override val value: String) extends Token(value)

case class PrivateVisibilityToken(override val value: String) extends Token(value)

case class PublicVisibilityToken(override val value: String) extends Token(value)

case class ProtectedVisibilityToken(override val value: String) extends Token(value)

case class ElementValueToken(override val value: String) extends Token(value)

trait TokenGenerator[T <: Token] {
  def createToken(input: String): Option[T]
}

class DirectMatchTokenGenerator[T <: Token](val pattern: Regex, val create: String => T) extends TokenGenerator[T] {
  def createToken(input: String): Option[T] = {
    pattern.findFirstIn(input).map(create)
  }
}

class UpToPatternTokenGenerator[T <: Token](val patterns: Set[Regex], val create: String => T) extends TokenGenerator[T] {
  def createToken(input: String): Option[T] = {
    @tailrec
    def findMatch(input: String, collected: String = ""): Option[String] = {
      if (input.isEmpty || patterns.exists(_.findFirstIn(input).isDefined)) {
        Option(collected).filter(_.nonEmpty)
      } else {
        findMatch(input.tail, collected + input.head)
      }
    }

    findMatch(input).map(create)
  }
}

class TokenGenerators {
  private val generators: List[TokenGenerator[_ <: Token]] = List(
    new DirectMatchTokenGenerator[StartToken](new Regex("^@startuml"), StartToken.apply),
    new DirectMatchTokenGenerator[LeftArrowToken](new Regex("^[<o*][|.-]?[.-]*(up|down|left|right)?[.-]+"), LeftArrowToken.apply),
    new DirectMatchTokenGenerator[RightArrowToken](new Regex("^[.-]+(up|down|left|right)?[.-]*[|.-]?[*o>]"), RightArrowToken.apply),
    new DirectMatchTokenGenerator[EndToken](new Regex("^@enduml"), EndToken.apply),
    new DirectMatchTokenGenerator[PackageToken](new Regex("^package"), PackageToken.apply),
    new DirectMatchTokenGenerator[ClassToken](new Regex("^class"), ClassToken.apply),
    new DirectMatchTokenGenerator[AbstractClassToken](new Regex("^abstract class"), AbstractClassToken.apply),
    new DirectMatchTokenGenerator[AbstractToken](new Regex("^abstract"), AbstractToken.apply),
    new DirectMatchTokenGenerator[InterfaceToken](new Regex("^interface"), InterfaceToken.apply),
    new DirectMatchTokenGenerator[EnumToken](new Regex("^enum"), EnumToken.apply),
    new DirectMatchTokenGenerator[NamespaceToken](new Regex("^namespace"), NamespaceToken.apply),
    new DirectMatchTokenGenerator[OpenCurlyBracketToken](new Regex("^\\{"), OpenCurlyBracketToken.apply),
    new DirectMatchTokenGenerator[CloseCurlyBracketToken](new Regex("^\\}"), CloseCurlyBracketToken.apply),
    new DirectMatchTokenGenerator[OpenRoundBracketToken](new Regex("^\\("), OpenRoundBracketToken.apply),
    new DirectMatchTokenGenerator[CloseRoundBracketToken](new Regex("^\\)"), CloseRoundBracketToken.apply),
    new DirectMatchTokenGenerator[ExtendsToken](new Regex("^extends"), ExtendsToken.apply),
    new DirectMatchTokenGenerator[ImplementsToken](new Regex("^implements"), ImplementsToken.apply),
    new DirectMatchTokenGenerator[PrivateVisibilityToken](new Regex("^-"), PrivateVisibilityToken.apply),
    new DirectMatchTokenGenerator[PublicVisibilityToken](new Regex("^\\+"), PublicVisibilityToken.apply),
    new DirectMatchTokenGenerator[ProtectedVisibilityToken](new Regex("^#"), ProtectedVisibilityToken.apply),
    new UpToPatternTokenGenerator[ElementValueToken](Set(
      // These regexes target whitespace and control characters that we don't need to interpret in PlantUML.
      new Regex("^ "),
      new Regex("^\\n"),
      new Regex("^\\r"),
      new Regex("^\\t"),
      // These regexes target PlantUML syntax elements that should interrupt the collection of an ElementValueToken.
      new Regex("^\\{"),
      new Regex("^\\}"),
      new Regex("^\\("),
      new Regex("^\\)"),
      new Regex("^\\:"),
      new Regex("^[<o*][|.-]?[.-]*(up|down|left|right)?[.-]+"),
      new Regex("^[.-]+(up|down|left|right)?[.-]*[|.-]?[*o>]"),
    ), ElementValueToken.apply)
  )

  def createToken(input: String): Option[_ <: Token] = {
    generators.flatMap(_.createToken(input)).headOption
  }
}

object Tokenizer {
  private val tokenGenerators = new TokenGenerators()

  def tokenize(input: String): List[Token] = {
    tokenizeInternal(input, List()).reverse
  }

  @tailrec
  private def tokenizeInternal(input: String, tokens: List[Token]): List[Token] = {
    val strippedInput = input.dropWhile(c => List(' ', '\n', '\r', '\t').contains(c))
    strippedInput match {
      case "" => tokens
      case _ => tokenGenerators.createToken(strippedInput) match {
        case Some(token) => tokenizeInternal(dropSubstring(strippedInput, token.value), token :: tokens)
        case None => tokens
      }
    }
  }

  private def dropSubstring(input: String, substring: String): String = {
    input.stripPrefix(substring)
  }
}
