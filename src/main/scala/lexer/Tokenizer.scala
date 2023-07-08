package lexer
import scala.annotation.tailrec
import scala.util.matching.Regex

abstract class Token(val value: String)
case class StartToken(override val value: String) extends Token(value)
case class EndToken(override val value: String) extends Token(value)
case class LeftArrowToken(override val value: String) extends Token(value)
case class RightArrowToken(override val value: String) extends Token(value)
case class ElementValueToken(override val value: String) extends Token(value)
class TokenGenerator[T <: Token](val pattern: Regex, val create: String => T) {
  def createToken(input: String): Option[T] = {
    pattern.findFirstIn(input).map(create)
  }
}

class TokenGenerators {
  private val generators: List[TokenGenerator[_ <: Token]] = List(
    new TokenGenerator[StartToken](new Regex("^@startuml"), StartToken.apply),
    new TokenGenerator[LeftArrowToken](new Regex("^[<o*][|.-]?[.-]*(up|down|left|right)?[.-]+"), LeftArrowToken.apply),
    new TokenGenerator[RightArrowToken](new Regex("^[.-]+(up|down|left|right)?[.-]*[|.-]?[*o>]"), RightArrowToken.apply),
    new TokenGenerator[EndToken](new Regex("^@enduml"), EndToken.apply),
    new TokenGenerator[ElementValueToken](new Regex("^[^\\s\\n\\r\\t<o*.|-]*"), ElementValueToken.apply)
  )

  def createToken(input: String): Option[Token] = {
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
