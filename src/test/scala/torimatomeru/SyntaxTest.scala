package torimatomeru

import org.parboiled2.ParseError
import org.scalatest.FunSpec

class SyntaxTest extends FunSpec {

  describe("ScalaSyntax") {
    it("Should simple vals") {
      {
        val s = new ScalaSyntax("val a = \"some literal\"")
        val res = s.PatVarDef.run()
        res.recover { case e: ParseError => println(s.formatError(e)) }
        assert(res.isSuccess)
      }
      {
        val s = new ScalaSyntax("val a = 12")
        val res = s.PatVarDef.run()
        res.recover { case e: ParseError => println(s.formatErrorProblem(e)) }
        assert(res.isSuccess)
      }
      {
        val s = new ScalaSyntax("val a = someVar")
        val res = s.PatVarDef.run()
        res.recover { case e: ParseError => println(s.formatErrorProblem(e)) }
        assert(res.isSuccess)
      }
    }
    it("Should simple vars") {

    }
    it("Should parse blocks") {

    }
  }
}