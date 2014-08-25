package torimatomeru

import org.parboiled2.ParseError

class IdentifierTest extends BaseTest {
  describe("Identifier matchers") {
    itShould("match simple words") {
      val s = new ScalaSyntax(s"""simpleWord""")
      val res = s.Id.run()
      res.recover { case e: ParseError => println(s.formatError(e) + "\n" + e.formatTraces) }
      assert(res.isSuccess)
      assert(s.cursor === s.input.length)
    }

    val keywords = Seq("abstract", "case", "catch", "class", "def", "do", "else", "extends", "false", "final", "finally", "for", "forSome", "if",
      "implicit", "import", "lazy", "match", "new", "null", "object", "override", "package", "private", "protected",
      "return", "sealed", "super", "this", "throw", "trait", "try", "true", "type", "val", "var", "while", "with", "yield", "_",
      ":", ";", "=", "=>", "<-", "<:", "<%", ">:", "#", "@", "\u21d2", "\u2190")

    for (keyword <- keywords) {
      itShould("reject keyword " + keyword) {
        val s = new ScalaSyntax(keyword + " ")
        import s._
        val res = s.Id.run()
        assert(s.cursor === 0)
        assert(res.isFailure)
      }
    }

    itShould("match ids starting with upercase") {
      val s = new ScalaSyntax(s"""Identifier""")
      val res = s.Id.run()
      res.recover { case e: ParseError => println(s.formatError(e) + "\n" + e.formatTraces) }
      assert(res.isSuccess)
      assert(s.cursor === s.input.length)
    }

    itShould("match several type of operators") {
      for (op <- Seq("<=", ">=", "==", "===", ":=", ":+", "+:", "~>", "!", "@%")) {
        val s = new ScalaSyntax(op)
        val res = s.Id.run()
        res.recover { case e: ParseError => println(s.formatError(e) + "\n" + e.formatTraces) }
        assert(res.isSuccess)
        assert(s.cursor === s.input.length)
      }
    }
  }
}