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
    it("Should accept comments where blanks are allowed") {
      {
        val s = new ScalaSyntax(s"""val a = someVar /*comment*/; val b = /*comment*/ 32;/*comment*/ 12 //lalal""")
        val res = s.Block.run()
        res.recover { case e: ParseError => println(s.formatErrorProblem(e)) }
        assert(res.isSuccess)
        assert(s.cursor === s.input.length)
      }
    }
    it("Should parse simple imports") {
      {
        val s = new ScalaSyntax(s"""import a.b.c, d.e""")
        val res = s.Block.run()
        res.recover { case e: ParseError => println(s.formatErrorProblem(e)) }
        assert(res.isSuccess)
        assert(s.cursor === s.input.length)
      }
    }
    it("Should parse import all") {
      {
        val s = new ScalaSyntax(s"""import a.b.c._""")
        val res = s.Block.run()
        res.recover { case e: ParseError => println(s.formatErrorProblem(e)) }
        assert(res.isSuccess)
        assert(s.cursor === s.input.length)
      }
    }
    it("Should parse selecting imports") {
      {
        val s = new ScalaSyntax(s"""import a.b.c.{d,e}""")
        val res = s.Block.run()
        res.recover { case e: ParseError => println(s.formatErrorProblem(e)) }
        assert(res.isSuccess)
        assert(s.cursor === s.input.length)
      }
    }
    it("Should parse selecting imports with aliases") {
      {
        val s = new ScalaSyntax(s"""import a.b.c.{d,e => f}""")
        val res = s.Block.run()
        res.recover { case e: ParseError => println(s.formatErrorProblem(e)) }
        assert(res.isSuccess)
        assert(s.cursor === s.input.length)
      }
    }
  }
}