package torimatomeru

import org.parboiled2.ParseError

class SyntaxTest extends BaseTest {

  describe("ScalaSyntax") {
    itShould("parse paths and stableId") {
      ruleSucceeds(s"""a.bc""")(_.Path)
      ruleSucceeds(s"""A.b.C""")(_.Path)
      ruleSucceeds(s"""a""")(_.Path)
      ruleSucceeds(s"""B""")(_.Path)
      ruleSucceeds(s"""this.B""")(_.Path)
      ruleSucceeds(s"""a.this.B""")(_.Path)
    }

    itShould("parse simple vals and vars") {
      for (d <- Seq("var", "val")) {
        ruleSucceeds(s"""$d a = "some literal"""")(_.PatVarDef)
        ruleSucceeds(s"""$d a = 12""")(_.PatVarDef)
        ruleSucceeds(s"""$d a = someVar""")(_.PatVarDef)
      }
    }

    itShould("parse simple vals and vars assigned to blocks") {
      for (d <- Seq("var", "val")) {
        ruleSucceeds(s"""$d a = {fakeId; 32}""")(_.PatVarDef)
      }
    }
    
    itShould("parse blocks") {
      ruleSucceeds(s"val a = someVar; val b = 32; 12")(_.Block)
    }
    itShould("accept comments where blanks are allowed") {
      ruleSucceeds(s"""val a = someVar /*comment*/; val b = /*comment*/ 32;/*comment*/ 12 //lalal""")(_.Block)
    }
    
    itShould("parse simple imports") {
      ruleSucceeds(s"import a.b.c, d.e ")(_.Import)
    }
    itShould("parse import all") {
      ruleSucceeds(s"""import a.b.c._;""")(_.Block)
    }
    itShould("parse selecting imports") {
      ruleSucceeds(s"""import a.b.c.{d,e};""")(_.Block)
    }
    itShould("parse selecting imports with aliases") {
      ruleSucceeds(s"""import a.b.c.{d,e => f};""")(_.Block)
    }

    itShould("parse patterns with variables") {
      ruleSucceeds(s"""a @ _""")(_.Pattern2)
    }
    itShould("parse patterns with deconstructors") {
      ruleSucceeds(s"""A(b, C(1,"hi"), 2)""")(_.Pattern)
    }
    itShould("parse patterns with types") {
      ruleSucceeds(s"""A(b: MyType)""")(_.Pattern)
    }
    itShould("parse very complicated patterns") {
      ruleSucceeds(s"""res@A(b: MyType, SomeB(d: Another, _, q@Query(a, c)))""")(_.Pattern)
    }
    itShould("parse simple case clauses") {
      ruleSucceeds(s"""case 2 => 1""")(_.CaseClause)
    }
    itShould("parse case clauses") {
      ruleSucceeds(s"""case res@A(b: MyType, SomeB(d: Another, _, q@Query(a, c))) => 1""")(_.CaseClause)
    }
    itShould("parse simple case clauses with simple guards") {
      ruleSucceeds(s"""case 1 if someCond => 1""")(_.CaseClause)
    }
    itShould("parse simple case clauses with simple guards 2") {
      ruleSucceeds(s"""case 1 if 4 <= 5 => 1""")(_.CaseClause)
    }
    itShould("parse simple case clauses with simple guards 3") {
      ruleSucceeds(s"""case 1 if 4 == 5 => 1""")(_.CaseClause)
    }
    itShould("parse type patterns") {
    	ruleSucceeds(s"""case a: Some => 1""")(_.CaseClause)
      ruleSucceeds(s"""case ex: Throwable => e.printStackTrace()""")(_.CaseClause)
    }
    itShould("parse case clauses with guards") {
      ruleSucceeds(s"""case res@A(b: MyType, SomeB(d: Another, _, q@Query(a, c))) if (a == c || someBoolean) => 1""")(_.CaseClause)
    }
  }
}