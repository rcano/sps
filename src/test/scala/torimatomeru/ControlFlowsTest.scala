package torimatomeru

import org.parboiled2._

class ControlFlowsTest extends BaseTest {

  describe("If control flow") {
    itShould("work for simple literals") {
      ruleSucceeds(s"""if (true) () else ()""")(_.IfCFlow)
    }
    itShould("work with variables as predicate") {
      ruleSucceeds(s"""if (cond) () else ()""")(_.IfCFlow)
    }
    itShould("work with joined predicate") {
      ruleSucceeds(s"""if (cond && cond2) () else ()""")(_.IfCFlow)
    }
    itShould("work with or-ed predicate") {
      ruleSucceeds(s"""if (cond || cond2) () else ()""")(_.IfCFlow)
    }
    itShould("work with nested predicate") {
      ruleSucceeds(s"""if ({var bool = true; bool}) () else ()""")(_.IfCFlow)
    }
    itShould("work without else") {
      ruleSucceeds(s"""if ({var bool = true; bool}) 42""")(_.IfCFlow)
    }
    itShould("work with nested conditions") {
      ruleSucceeds(s"""if ({if (cond) true else false}) 42""")(_.IfCFlow)
    }
  }

  describe("for control flow") {
	  itShould("work with one generator") {
		  ruleSucceeds(s"""for (a <- b) ()""")(_.ForCFlow)
	  }
    itShould("work with yields") {
      ruleSucceeds(s"""for (a <- b) yield ()""")(_.ForCFlow)
    }
    itShould("work with multiple generators separated by ;") {
      ruleSucceeds(s"""for (a <- b; a2 <- b2; a3 <- b3) ()""")(_.ForCFlow)
    }
    itShould("work with multiple generators separated by newline") {
      ruleSucceeds(s"""for {a <- b
		     a2 <- b2
		     a3 <- b3} ()""")(_.ForCFlow)
    }
    itShould("work with guards") {
    	ruleSucceeds(s"""for (a <- b if someCond) ()""")(_.ForCFlow)
    }
    itShould("work with complex guards") {
    	ruleSucceeds(s"""for (a <- b if someCond || varA == varB && (true || false)) ()""")(_.ForCFlow)
    }
    itShould("work with several guards") {
    	ruleSucceeds(s"""for (a <- b if someCond 
    			if varA == varB && (true || false)) ()""")(_.ForCFlow)
    }
    itShould("work with nested fors") {
      ruleSucceeds(s"""for (a <- b if someCond 
        c <- for (d <- gen if cond) yield something) ()""")(_.ForCFlow)
    }
  }
  
  describe("while control flow") {
	  itShould("work with simple conditions") {
		  ruleSucceeds(s"""while (true) ()""")(_.WhileCFlow)
	  }
	  itShould("work with simple conditions 2") {
		  ruleSucceeds(s"""while (cond && a == b) ()""")(_.WhileCFlow)
	  }
	  itShould("work with complex conditions ") {
		  ruleSucceeds(s"""while (cond && {i += 1; i < someLimit}) ()""")(_.WhileCFlow)
	  }
  }
  
  describe("do while control flow") {
	  itShould("work with simple conditions") {
		  ruleSucceeds(s"""do someMethod() while (true)""")(_.DoWhileCFlow)
	  }
	  itShould("work with simple conditions 2") {
		  ruleSucceeds(s"""do {someMethod()
		    someOtherThing()
		    a == b} while (cond && a == b)""")(_.DoWhileCFlow)
	  }
    itShould("work with complex conditions ") {
		  ruleSucceeds(s"""do () while (cond && {i += 1; i < someLimit})""")(_.DoWhileCFlow)
    }
  }
  
  describe("try control flow") {
	  itShould("work with one statement") {
		  ruleSucceeds(s"""try {something} catch { case ex => ex.printStackTrace() }""")(_.TryCFlow)
	  }
	  itShould("work without catch with just finally") {
		  ruleSucceeds(s"""try {something} finally something""")(_.TryCFlow)
	  }
    itShould("work with catch and finally") {
		  ruleSucceeds(s"""try {something} catch { case ex => ex.printStackTrace(); stat2 } finally something""")(_.TryCFlow)
	  }
  }
}