package torimatomeru
package syntax

import org.parboiled2._

trait Identifiers { self: Parser with Basic =>

  def Operator = rule(oneOrMore(OperatorChar))
  
  def VarId = rule { !(Keywords ~ (WhitespaceChar | Newline)) ~ Lower ~ IdRest }
  def PlainId = rule { VarId | !(Keywords ~ (WhitespaceChar | Newline)) ~ (Upper ~ IdRest | Operator) }
  def Id = rule { PlainId | ("`" ~ oneOrMore(ANY) ~ "`") }
  def IdRest = rule { zeroOrMore(Letter | Digit) ~ optional("_" ~ Operator) }

  
  def Keywords = rule {
    "abstract" | "case" | "catch" | "class" | "def" | "do" | "else" | "extends" | "false" | "final" | "finally" | "for" | "forSome" | "if" |
    "implicit" | "import" | "lazy" | "match" | "new" | "null" | "object" | "override" | "package" | "private" | "protected" | "return" |
    "return" | "sealed" | "super" | "this" | "throw" | "trait" | "try" | "true" | "type" | "val" | "var" | "while" | "with" | "yield" | "_" |
    ":" | ";" | "=" | "=>" | "<-" | "<:" | "<%" | ">:" | "#" | "@" | "\u21d2" | "\u2190"
  }
}
