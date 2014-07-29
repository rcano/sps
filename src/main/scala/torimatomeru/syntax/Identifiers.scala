package torimatomeru
package syntax

import org.parboiled2._

trait Identifiers { self: ScalaSyntax =>

  def Operator = rule(oneOrMore(OperatorChar))
  
  def VarId = rule { !Keywords ~ Lower ~ IdRest }
  def PlainId = rule { !Keywords ~ Upper ~ IdRest | VarId | Operator }
  def Id = rule { PlainId | ("`" ~ oneOrMore(ANY) ~ "`") }
  def IdRest = rule { zeroOrMore(Letter | Digit) ~ optional("_" ~ Operator) }

  def Keywords = rule {
    "abstract" | "case" | "catch" | "class" | "def" | "do" | "else" | "extends" | "false" | "final" | "finally" | "for" | "forSome" | "if" |
    "implicit" | "import" | "lazy" | "match" | "new" | "null" | "object" | "override" | "package" | "private" | "protected" | "return" |
    "return" | "sealed" | "super" | "this" | "throw" | "trait" | "try" | "true" | "type" | "val" | "var" | "while" | "with" | "yield" | "_" |
    ":" | "=" | "=>" | "<-" | "<:" | "<%" | ">:" | "#" | "@" | "\u21d2" | "\u2190"
  }
}
