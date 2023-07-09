package lexer

import scala.util.matching.Regex

abstract class Token(val value: String)

case class StartToken(override val value: String) extends Token(value)

object StartToken {
  val regex: Regex = "^@startuml".r
}

case class EndToken(override val value: String) extends Token(value)

object EndToken {
  val regex: Regex = "^@enduml".r
}

case class LeftArrowToken(override val value: String) extends Token(value)

object LeftArrowToken {
  val regex: Regex = "^[<o*][|.-]?[.-]*(up|down|left|right)?[.-]+".r
}

case class RightArrowToken(override val value: String) extends Token(value)

object RightArrowToken {
  val regex: Regex = "^[.-]+(up|down|left|right)?[.-]*[|.-]?[*o>]".r
}

case class PackageToken(override val value: String) extends Token(value)

object PackageToken {
  val regex: Regex = "^package".r
}

case class ClassToken(override val value: String) extends Token(value)

object ClassToken {
  val regex: Regex = "^class".r
}

case class AbstractClassToken(override val value: String) extends Token(value)

object AbstractClassToken {
  val regex: Regex = "^abstract class".r
}

case class AbstractToken(override val value: String) extends Token(value)

object AbstractToken {
  val regex: Regex = "^abstract".r
}

case class InterfaceToken(override val value: String) extends Token(value)

object InterfaceToken {
  val regex: Regex = "^interface".r
}

case class EnumToken(override val value: String) extends Token(value)

object EnumToken {
  val regex: Regex = "^enum".r
}

case class NamespaceToken(override val value: String) extends Token(value)

object NamespaceToken {
  val regex: Regex = "^namespace".r
}

case class OpenCurlyBracketToken(override val value: String) extends Token(value)

object OpenCurlyBracketToken {
  val regex: Regex = "^\\{".r
}

case class CloseCurlyBracketToken(override val value: String) extends Token(value)

object CloseCurlyBracketToken {
  val regex: Regex = "^}".r
}

case class OpenRoundBracketToken(override val value: String) extends Token(value)

object OpenRoundBracketToken {
  val regex: Regex = "^\\(".r
}

case class CloseRoundBracketToken(override val value: String) extends Token(value)

object CloseRoundBracketToken {
  val regex: Regex = "^\\)".r
}

case class ExtendsToken(override val value: String) extends Token(value)

object ExtendsToken {
  val regex: Regex = "^extends".r
}

case class ImplementsToken(override val value: String) extends Token(value)

object ImplementsToken {
  val regex: Regex = "^implements".r
}

case class PrivateVisibilityToken(override val value: String) extends Token(value)

object PrivateVisibilityToken {
  val regex: Regex = "^-".r
}

case class PublicVisibilityToken(override val value: String) extends Token(value)

object PublicVisibilityToken {
  val regex: Regex = "^\\+".r
}

case class ProtectedVisibilityToken(override val value: String) extends Token(value)

object ProtectedVisibilityToken {
  val regex: Regex = "^#".r
}

case class ColonToken(override val value: String) extends Token(value)

object ColonToken {
  val regex: Regex = "^:".r
}

case class ElementValueToken(override val value: String) extends Token(value)

object ElementValueToken {
  val tokenEndPatterns: Set[Regex] = Set(
    // These regexes target whitespace and control characters that we don't need to interpret in PlantUML.
    "^ ".r,
    "^\\n".r,
    "^\\r".r,
    "^\\t".r,
    // These regexes target PlantUML syntax elements that should interrupt the collection of an ElementValueToken.
    "^\\{".r,
    "^}".r,
    "^\\(".r,
    "^\\)".r,
    "^:".r,
    "^[<o*][|.-]?[.-]*(up|down|left|right)?[.-]+".r,
    "^[.-]+(up|down|left|right)?[.-]*[|.-]?[*o>]".r,
  )
}