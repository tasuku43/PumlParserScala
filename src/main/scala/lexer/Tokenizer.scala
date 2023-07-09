package lexer

import scala.collection.immutable.List

object Tokenizer {
  private val tokenGenerators = TokenGenerators.factory()

  def tokenize(input: String): List[Token] = {
    tokenizeInternal(input, List()).reverse
  }

  private def tokenizeInternal(input: String, tokens: List[Token]): List[Token] = {
    val strippedInput = input.dropWhile(c => List(' ', '\n', '\r', '\t').contains(c))
    tokenGenerators.createToken(strippedInput).fold(tokens) { token =>
      tokenizeInternal(dropSubstring(strippedInput, token.value), token :: tokens)
    }
  }

  private def dropSubstring(input: String, substring: String): String = {
    input.stripPrefix(substring)
  }
}
