package torimatomeru

import org.parboiled2.ParseError

class IdentifierTest extends BaseTest {
  describe("Identifier matchers") {
    itShould("match simple words") {
      ruleSucceeds(s"""simpleWord""")(_.Id)
    }

    val keywords = Seq("abstract", "case", "catch", "class", "def", "do", "else", "extends", "false", "final", "finally", "for", "forSome", "if",
      "implicit", "import", "lazy", "match", "new", "null", "object", "override", "package", "private", "protected",
      "return", "sealed", "super", "this", "throw", "trait", "try", "true", "type", "val", "var", "while", "with", "yield", "_",
      ":", ";", "=", "=>", "<-", "<:", "<%", ">:", "#", "@", "\u21d2", "\u2190")

    for (keyword <- keywords) {
      itShould("reject keyword " + keyword) {
        ruleFails(keyword + " ")(_.Id)
      }
    }

    itShould("match ids starting with upercase") {
      ruleSucceeds(s"""Identifier""")(_.Id)
    }

    itShould("match several type of operators") {
      for (op <- Seq("<=", ">=", "==", "===", ":=", ":+", "+:", "~>", "!", "@%")) {
        ruleSucceeds(op)(_.Id)
      }
    }
  }
}