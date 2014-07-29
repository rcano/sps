package torimatomeru

import org.parboiled2.ParseError
import org.scalatest.FunSpec

class SyntaxTest extends FunSpec {

  describe("ScalaSyntax") {
    it("Should parse simple vals and vars") {
      for (d <- Seq("var", "val")) {
        {
          val s = new ScalaSyntax(s"""$d a = "some literal"""")
          val res = s.PatVarDef.run()
          res.recover { case e: ParseError => println(s.formatError(e)) }
          assert(res.isSuccess)
          assert(s.cursor === s.input.length)
        }
        {
          val s = new ScalaSyntax(s"$d a = 12")
          val res = s.PatVarDef.run()
          res.recover { case e: ParseError => println(s.formatErrorProblem(e)) }
          assert(res.isSuccess)
          assert(s.cursor === s.input.length)
        }
        {
          val s = new ScalaSyntax(s"$d a = someVar")
          val res = s.PatVarDef.run()
          res.recover { case e: ParseError => println(s.formatErrorProblem(e)) }
          assert(res.isSuccess)
          assert(s.cursor === s.input.length)
        }
      }
    }
    it("Should parse simple vals and vars assigned to blocks") {
      for (d <- Seq("var", "val")) {
        {
          val s = new ScalaSyntax(s"""$d a = {fakeId; 32}""")
          val res = s.PatVarDef.run()
          res.recover { case e: ParseError => println(s.formatError(e)) }
          assert(res.isSuccess)
        }
      }
    }
    it("Should parse blocks") {
      {
        val s = new ScalaSyntax(s"val a = someVar; val b = 32; 12")
        val res = s.Block.run()
        res.recover { case e: ParseError => println(s.formatErrorProblem(e)) }
        assert(res.isSuccess)
        assert(s.cursor === s.input.length)
      }
    }
  }
}