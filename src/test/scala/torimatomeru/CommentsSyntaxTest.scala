package torimatomeru

import org.parboiled2.ParseError

class CommentsSyntaxTest extends BaseTest {

  describe("Comments rules") {
    itShould("match simple comments") {
      val s = new ScalaSyntax(s"""//someComment here, even with another // in it""")
      val res = s.Comment.run()
      res.recover { case e: ParseError => println(s.formatError(e)) }
      assert(res.isSuccess)
      assert(s.cursor === s.input.length)
    }
    itShould("match multiline comments") {
      val s = new ScalaSyntax(s"""/*
here is another comment
*/""")
      val res = s.Comment.run()
      res.recover { case e: ParseError => println(s.formatError(e)) }
      assert(res.isSuccess)
      assert(s.cursor === s.input.length)
    }
    itShould("match nested multiline comments") {
      val s = new ScalaSyntax(s"""/*
here is another /*comment*/ /*and/*a/*very/*nested/*one*/*/*/*/*/
*/""")
      val res = s.Comment.run()
      res.recover { case e: ParseError => println(s.formatError(e)) }
      assert(res.isSuccess)
      assert(s.cursor === s.input.length)
    }
    itShould("match comments right after a Semi") {
      val s = new ScalaSyntax(s""";/*some comment*/""")
      val res = s.Semi.run()
      res.recover { case e: ParseError => println(s.formatError(e)) }
      assert(res.isSuccess)
      assert(s.cursor === s.input.length)
    }
  }
}