package torimatomeru

import org.parboiled2._

class ControlFlowsTest extends BaseTest {

  describe("If control flow") {
    itShould("work for simple literals") {
      val s = new ScalaSyntax(s"""if (true) () else ()""")
      val res = s.IfCFlow.run()
      res.recover { case e: ParseError => println(s.formatError(e) + "\n" + e.formatTraces) }
      assert(res.isSuccess)
      assert(s.cursor === s.input.length)
    }
    itShould("work with variables as predicate") {
      val s = new ScalaSyntax(s"""if (cond) () else ()""")
      val res = s.IfCFlow.run()
      res.recover { case e: ParseError => println(s.formatError(e) + "\n" + e.formatTraces) }
      assert(res.isSuccess)
      assert(s.cursor === s.input.length)
    }
    itShould("work with joined predicate") {
      val s = new ScalaSyntax(s"""if (cond && cond2) () else ()""")
      val res = s.IfCFlow.run()
      res.recover { case e: ParseError => println(s.formatError(e) + "\n" + e.formatTraces) }
      assert(res.isSuccess)
      assert(s.cursor === s.input.length)
    }
    itShould("work with or-ed predicate") {
      val s = new ScalaSyntax(s"""if (cond || cond2) () else ()""")
      val res = s.IfCFlow.run()
      res.recover { case e: ParseError => println(s.formatError(e) + "\n" + e.formatTraces) }
      assert(res.isSuccess)
      assert(s.cursor === s.input.length)
    }
    itShould("work with nested predicate") {
      val s = new ScalaSyntax(s"""if ({var bool = true; bool}) () else ()""")
      val res = s.IfCFlow.run()
      res.recover { case e: ParseError => println(s.formatError(e) + "\n" + e.formatTraces) }
      assert(res.isSuccess)
      assert(s.cursor === s.input.length)
    }
    itShould("work without else") {
      val s = new ScalaSyntax(s"""if ({var bool = true; bool}) 42""")
      val res = s.IfCFlow.run()
      res.recover { case e: ParseError => println(s.formatError(e) + "\n" + e.formatTraces) }
      assert(res.isSuccess)
      assert(s.cursor === s.input.length)
    }
    itShould("work with nested conditions") {
      val s = new ScalaSyntax(s"""if ({if (cond) true else false}) 42""")
      val res = s.IfCFlow.run()
      res.recover { case e: ParseError => println(s.formatError(e) + "\n" + e.formatTraces) }
      assert(res.isSuccess)
      assert(s.cursor === s.input.length)
    }
  }

  describe("for control flow") {
	  itShould("work with one generator") {
		  val s = new ScalaSyntax(s"""for (a <- b) ()""")
		  val res = s.ForCFlow.run()
		  res.recover { case e: ParseError => println(s.formatError(e) + "\n" + e.formatTraces) }
		  assert(res.isSuccess)
		  assert(s.cursor === s.input.length)
	  }
    itShould("work with yields") {
      val s = new ScalaSyntax(s"""for (a <- b) yield ()""")
      val res = s.ForCFlow.run()
      res.recover { case e: ParseError => println(s.formatError(e) + "\n" + e.formatTraces) }
      assert(res.isSuccess)
      assert(s.cursor === s.input.length)
    }
    itShould("work with multiple generators separated by ;") {
      val s = new ScalaSyntax(s"""for (a <- b; a2 <- b2; a3 <- b3) ()""")
      val res = s.ForCFlow.run()
      res.recover { case e: ParseError => println(s.formatError(e) + "\n" + e.formatTraces) }
      assert(res.isSuccess)
      assert(s.cursor === s.input.length)
    }
    itShould("work with multiple generators separated by newline") {
      val s = new ScalaSyntax(s"""for {a <- b
		     a2 <- b2
		     a3 <- b3} ()""")
      val res = s.ForCFlow.run()
      res.recover { case e: ParseError => println(s.formatError(e) + "\n" + e.formatTraces) }
      assert(res.isSuccess)
      assert(s.cursor === s.input.length)
    }
    itShould("work with guards") {
    	val s = new ScalaSyntax(s"""for (a <- b if someCond) ()""")
    	val res = s.ForCFlow.run()
    	res.recover { case e: ParseError => println(s.formatError(e) + "\n" + e.formatTraces) }
    	assert(res.isSuccess)
    	assert(s.cursor === s.input.length)
    }
    itShould("work with complex guards") {
    	val s = new ScalaSyntax(s"""for (a <- b if someCond || varA == varB && (true || false)) ()""")
    	val res = s.ForCFlow.run()
    	res.recover { case e: ParseError => println(s.formatError(e) + "\n" + e.formatTraces) }
    	assert(res.isSuccess)
    	assert(s.cursor === s.input.length)
    }
    itShould("work with several guards") {
    	val s = new ScalaSyntax(s"""for (a <- b if someCond 
    			if varA == varB && (true || false)) ()""")
    	val res = s.ForCFlow.run()
    	res.recover { case e: ParseError => println(s.formatError(e) + "\n" + e.formatTraces) }
    	assert(res.isSuccess)
    	assert(s.cursor === s.input.length)
    }
    itShould("work with nested fors") {
      val s = new ScalaSyntax(s"""for (a <- b if someCond 
        c <- for (d <- gen if cond) yield something) ()""")
      val res = s.ForCFlow.run()
      res.recover { case e: ParseError => println(s.formatError(e) + "\n" + e.formatTraces) }
      assert(res.isSuccess)
      assert(s.cursor === s.input.length)
    }
  }
  
  describe("while control flow") {
	  itShould("work with simple conditions") {
		  val s = new ScalaSyntax(s"""while (true) ()""")
		  val res = s.WhileCFlow.run()
		  res.recover { case e: ParseError => println(s.formatError(e) + "\n" + e.formatTraces) }
		  assert(res.isSuccess)
		  assert(s.cursor === s.input.length)
	  }
	  itShould("work with simple conditions 2") {
		  val s = new ScalaSyntax(s"""while (cond && a == b) ()""")
		  val res = s.WhileCFlow.run()
		  res.recover { case e: ParseError => println(s.formatError(e) + "\n" + e.formatTraces) }
		  assert(res.isSuccess)
		  assert(s.cursor === s.input.length)
	  }
	  itShould("work with complex conditions ") {
		  val s = new ScalaSyntax(s"""while (cond && {i += 1; i < someLimit}) ()""")
		  val res = s.WhileCFlow.run()
		  res.recover { case e: ParseError => println(s.formatError(e) + "\n" + e.formatTraces) }
		  assert(res.isSuccess)
		  assert(s.cursor === s.input.length)
	  }
  }
  
  describe("do while control flow") {
	  itShould("work with simple conditions") {
		  val s = new ScalaSyntax(s"""do someMethod() while (true)""")
		  val res = s.DoWhileCFlow.run()
		  res.recover { case e: ParseError => println(s.formatError(e) + "\n" + e.formatTraces) }
		  assert(res.isSuccess)
		  assert(s.cursor === s.input.length)
	  }
	  itShould("work with simple conditions 2") {
		  val s = new ScalaSyntax(s"""do {someMethod()
		    someOtherThing()
		    a == b} while (cond && a == b)""")
		  val res = s.DoWhileCFlow.run()
		  res.recover { case e: ParseError => println(s.formatError(e) + "\n" + e.formatTraces) }
		  assert(res.isSuccess)
		  assert(s.cursor === s.input.length)
	  }
    itShould("work with complex conditions ") {
		  val s = new ScalaSyntax(s"""do () while (cond && {i += 1; i < someLimit})""")
		  val res = s.DoWhileCFlow.run()
		  res.recover { case e: ParseError => println(s.formatError(e) + "\n" + e.formatTraces) }
		  assert(res.isSuccess)
		  assert(s.cursor === s.input.length)
    }
  }
  
  describe("try control flow") {
	  itShould("work with one statement") {
		  val s = new ScalaSyntax(s"""try {something} catch { case ex => ex.printStackTrace() }""")
		  val res = s.TryCFlow.run()
		  res.recover { case e: ParseError => println(s.formatError(e) + "\n" + e.formatTraces) }
		  assert(res.isSuccess)
		  assert(s.cursor === s.input.length)
	  }
	  itShould("work without catch with just finally") {
		  val s = new ScalaSyntax(s"""try {something} finally something""")
		  val res = s.TryCFlow.run()
		  res.recover { case e: ParseError => println(s.formatError(e) + "\n" + e.formatTraces) }
		  assert(res.isSuccess)
		  assert(s.cursor === s.input.length)
	  }
    itShould("work with catch and finally") {
		  val s = new ScalaSyntax(s"""try {something} catch { case ex => ex.printStackTrace(); stat2 } finally something""")
		  val res = s.TryCFlow.run()
		  res.recover { case e: ParseError => println(s.formatError(e) + "\n" + e.formatTraces) }
		  assert(res.isSuccess)
		  assert(s.cursor === s.input.length)
	  }
  }
}