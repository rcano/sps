package torimatomeru

import org.parboiled2.ParseError

class SyntaxTest extends BaseTest {

  describe("ScalaSyntax") {
    itShould("parse paths and stableId") {
      {
        val s = new ScalaSyntax(s"""a.bc""")
        val res = s.Path.run()
        res.recover { case e: ParseError => println(s.formatError(e) + "\n" + e.formatTraces) }
        assert(res.isSuccess)
        assert(s.cursor === s.input.length)
      }
      {
        val s = new ScalaSyntax(s"""A.b.C""")
        val res = s.Path.run()
        res.recover { case e: ParseError => println(s.formatError(e) + "\n" + e.formatTraces) }
        assert(res.isSuccess)
        assert(s.cursor === s.input.length)
      }
      {
        val s = new ScalaSyntax(s"""a""")
        val res = s.Path.run()
        res.recover { case e: ParseError => println(s.formatError(e) + "\n" + e.formatTraces) }
        assert(res.isSuccess)
        assert(s.cursor === s.input.length)
      }
      {
        val s = new ScalaSyntax(s"""B""")
        val res = s.Path.run()
        res.recover { case e: ParseError => println(s.formatError(e) + "\n" + e.formatTraces) }
        assert(res.isSuccess)
        assert(s.cursor === s.input.length)
      }
      {
        val s = new ScalaSyntax(s"""this.B""")
        val res = s.Path.run()
        res.recover { case e: ParseError => println(s.formatError(e) + "\n" + e.formatTraces) }
        assert(res.isSuccess)
        assert(s.cursor === s.input.length)
      }
      {
        val s = new ScalaSyntax(s"""a.this.B""")
        val res = s.Path.run()
        res.recover { case e: ParseError => println(s.formatError(e) + "\n" + e.formatTraces) }
        assert(res.isSuccess)
        assert(s.cursor === s.input.length)
      }
    }
    itShould("parse simple vals and vars") {
      for (d <- Seq("var", "val")) {
        {
          val s = new ScalaSyntax(s"""$d a = "some literal"""")
          val res = s.PatVarDef.run()
          res.recover { case e: ParseError => println(s.formatError(e) + "\n" + e.formatTraces) }
          assert(res.isSuccess)
          assert(s.cursor === s.input.length)
        }
        {
          val s = new ScalaSyntax(s"$d a = 12")
          val res = s.PatVarDef.run()
          res.recover { case e: ParseError => println(s.formatError(e) + "\n" + e.formatTraces) }
          assert(res.isSuccess)
          assert(s.cursor === s.input.length)
        }
        {
          val s = new ScalaSyntax(s"$d a = someVar")
          val res = s.PatVarDef.run()
          res.recover { case e: ParseError => println(s.formatError(e) + "\n" + e.formatTraces) }
          assert(res.isSuccess)
          assert(s.cursor === s.input.length)
        }
      }
    }
    itShould("parse simple vals and vars assigned to blocks") {
      for (d <- Seq("var", "val")) {
        val s = new ScalaSyntax(s"""$d a = {fakeId; 32}""")
        val res = s.PatVarDef.run()
        res.recover { case e: ParseError => println(s.formatError(e) + "\n" + e.formatTraces) }
        assert(res.isSuccess)
      }
    }
    itShould("parse blocks") {
      val s = new ScalaSyntax(s"val a = someVar; val b = 32; 12")
      val res = s.Block.run()
      res.recover { case e: ParseError => println(s.formatError(e) + "\n" + e.formatTraces) }
      assert(res.isSuccess)
      assert(s.cursor === s.input.length)
    }
    itShould("accept comments where blanks are allowed") {
      val s = new ScalaSyntax(s"""val a = someVar /*comment*/; val b = /*comment*/ 32;/*comment*/ 12 //lalal""")
      val res = s.Block.run()
      res.recover { case e: ParseError => println(s.formatError(e) + "\n" + e.formatTraces) }
      assert(res.isSuccess)
      assert(s.cursor === s.input.length)
    }
    itShould("parse simple imports") {
      val s = new ScalaSyntax(s"import a.b.c, d.e\n")
      val res = s.Import.run()
      res.recover { case e: ParseError => println(s.formatError(e) + "\n" + e.formatTraces) }
      assert(res.isSuccess)
      assert(s.cursor === s.input.length)
    }
    itShould("parse import all") {
      val s = new ScalaSyntax(s"""import a.b.c._;""")
      val res = s.Block.run()
      res.recover { case e: ParseError => println(s.formatError(e) + "\n" + e.formatTraces) }
      assert(res.isSuccess)
      assert(s.cursor === s.input.length)
    }
    itShould("parse selecting imports") {
      val s = new ScalaSyntax(s"""import a.b.c.{d,e};""")
      val res = s.Block.run()
      res.recover { case e: ParseError => println(s.formatError(e) + "\n" + e.formatTraces) }
      assert(res.isSuccess)
      assert(s.cursor === s.input.length)
    }
    itShould("parse selecting imports with aliases") {
      val s = new ScalaSyntax(s"""import a.b.c.{d,e => f};""")
      val res = s.Block.run()
      res.recover { case e: ParseError => println(s.formatError(e) + "\n" + e.formatTraces) }
      assert(res.isSuccess)
      assert(s.cursor === s.input.length)
    }

    itShould("parse patterns with variables") {
      val s = new ScalaSyntax(s"""a @ _""")
      val res = s.Pattern2.run()
      res.recover { case e: ParseError => println(s.formatError(e) + "\n" + e.formatTraces) }
      assert(res.isSuccess)
      assert(s.cursor === s.input.length)
    }
    itShould("parse patterns with deconstructors") {
      val s = new ScalaSyntax(s"""A(b, C(1,"hi"), 2)""")
      val res = s.Pattern.run()
      res.recover { case e: ParseError => println(s.formatError(e) + "\n" + e.formatTraces) }
      assert(res.isSuccess)
      assert(s.cursor === s.input.length)
    }
    itShould("parse patterns with types") {
      val s = new ScalaSyntax(s"""A(b: MyType)""")
      val res = s.Pattern.run()
      res.recover { case e: ParseError => println(s.formatError(e) + "\n" + e.formatTraces) }
      assert(res.isSuccess)
      assert(s.cursor === s.input.length)
    }
    itShould("parse very complicated patterns") {
      val s = new ScalaSyntax(s"""res@A(b: MyType, SomeB(d: Another, _, q@Query(a, c)))""")
      val res = s.Pattern.run()
      res.recover { case e: ParseError => println(s.formatError(e) + "\n" + e.formatTraces) }
      assert(res.isSuccess)
      assert(s.cursor === s.input.length)
    }
    itShould("parse case clauses") {
      val s = new ScalaSyntax(s"""case res@A(b: MyType, SomeB(d: Another, _, q@Query(a, c))) => 1""")
      val res = s.CaseClause.run()
      res.recover { case e: ParseError => println(s.formatError(e) + "\n" + e.formatTraces) }
      assert(res.isSuccess)
      assert(s.cursor === s.input.length)
    }
    itShould("parse case clauses with guards") {
      val s = new ScalaSyntax(s"""case res@A(b: MyType, SomeB(d: Another, _, q@Query(a, c))) if (a == c || someBoolean) => 1""")
      val res = s.CaseClause.run()
      res.recover { case e: ParseError => println(s.formatError(e) + "\n" + e.formatTraces) }
      assert(res.isSuccess)
      assert(s.cursor === s.input.length)
    }
  }
}