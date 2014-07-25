package torimatomeru

import org.parboiled2._
import org.scalatest.FunSpec
import scala.util._

class ParserTest extends FunSpec {

  def prettyPrintException[R](s: ScalaSyntax, t: Try[R]) = t match {
    case Success(s) => s.toString
    case Failure(p: ParseError) => s.formatError(p)
    case Failure(other) => other.toString
  }

  def syntaxShould(descr: String)(input: String)(test: (String, ScalaSyntax) => Unit): Unit = it(descr) {
    val s = new ScalaSyntax(input)
    test(input, s)
  }

  describe("CharacterLiteral parser") {
    syntaxShould("should match simple chars")("'a'".trim) { (i, s) =>
      val res = s.CharacterLiteral.run()
      assert(res.isSuccess, "Expected success but was " + prettyPrintException(s, res))
      assert(res.get == i.drop(1).dropRight(1))
    }
    syntaxShould("should match escaped chars")("'\\n'".trim) { (i, s) =>
      val res = s.CharacterLiteral.run()
      assert(res.isSuccess, "Expected success but was " + prettyPrintException(s, res))
      assert(res.get == i.drop(1).dropRight(1))
    }
    syntaxShould("should match utf8")("'↓'".trim) { (i, s) =>
      val res = s.CharacterLiteral.run()
      assert(res.isSuccess, "Expected success but was " + prettyPrintException(s, res))
      assert(res.get == i.drop(1).dropRight(1))
    }
    syntaxShould("should match escaped unicodes")("'\\u0030'".trim) { (i, s) =>
      val res = s.CharacterLiteral.run()
      assert(res.isSuccess, "Expected success but was " + prettyPrintException(s, res))
      assert(res.get == i.drop(1).dropRight(1))
    }
    syntaxShould("should failed for \\ alone")("'\\'".trim) { (i, s) =>
      val res = s.CharacterLiteral.run()
      assert(res.isFailure, "Expected failure but was " + prettyPrintException(s, res))
    }
  }

  describe("StringLiteral parser") {
    syntaxShould("works for simple strings without quotes")(""" "hello world!" """.trim) { (i, s) =>
      val res = s.StringLiteral.run()
      assert(res.isSuccess)
      assert(res.get == i.drop(1).dropRight(1))
    }
    syntaxShould("accepts simple string with escaped quotes")(""" "hello\"world!" """.trim) { (i, s) =>
      val res = s.StringLiteral.run()
      assert(res.isSuccess)
      assert(res.get == i.drop(1).dropRight(1))
    }
    syntaxShould("rejects simple strings with newlines")(""" "hello
world!" """.trim) { (i, s) =>
      val res = s.StringLiteral.run()
      assert(res.isFailure)
    }
    syntaxShould("works for literal strings")(" \"\"\"hello world!\"\"\" ".trim) { (i, s) =>
      val res = s.StringLiteral.run()
      assert(res.isSuccess, res.toString)
      assert(res.get == i.drop(3).dropRight(3))
    }
    syntaxShould("works for literal strings with newLines")(" \"\"\"hello\nworld!\"\"\" ".trim) { (i, s) =>
      val res = s.StringLiteral.run()
      assert(res.isSuccess, res.toString)
      assert(res.get == i.drop(3).dropRight(3))
    }
    syntaxShould("works for literal strings which end in several double quotes")(" \"\"\"hello\nworld!\"\"\"\"\"\" ".trim) { (i, s) =>
      val res = s.StringLiteral.run()
      assert(res.isSuccess, res.toString)
      assert(res.get == i.drop(3).dropRight(3))
    }
    syntaxShould("works for literal strings which start with quotes and end in several double quotes")(" \"\"\"\"\"hello\nworld!\"\"\"\"\"\" ".trim) { (i, s) =>
      val res = s.StringLiteral.run()
      assert(res.isSuccess, res.toString)
      assert(res.get == i.drop(3).dropRight(3))
    }
    syntaxShould("works with utf8 strings")(""" "今日は！" """.trim) { (i, s) =>
      val res = s.StringLiteral.run()
      assert(res.isSuccess)
      assert(res.get == i.drop(1).dropRight(1))
    }
  }

  describe("Literals") {
    it("work for booleans") {
      var s = new ScalaSyntax("true")
      var res = s.BooleanLiteral.run()
      assert(res.isSuccess)
      s = new ScalaSyntax("false")
      res = s.BooleanLiteral.run()
      assert(res.isSuccess)
    }

    it("should work for literal integers (and longs)") {
      assert(new ScalaSyntax("1").IntegerLiteral.run().isSuccess)
      assert(new ScalaSyntax("123123123").IntegerLiteral.run().isSuccess)
      assert(new ScalaSyntax("123123123l").IntegerLiteral.run().isSuccess)
      assert(new ScalaSyntax("123123123123123L").IntegerLiteral.run().isSuccess)
      assert(new ScalaSyntax("12312312312.231").IntegerLiteral.run().get == "12312312312")
    }

    it("should work for literal floating point numbers") {
      assert(new ScalaSyntax("1").FloatingPointLiteral.run().isFailure)
      assert(new ScalaSyntax("12312312312.231").FloatingPointLiteral.run().get == "12312312312.231")
      assert(new ScalaSyntax(".231").FloatingPointLiteral.run().get == ".231")
      assert(new ScalaSyntax(".231f").FloatingPointLiteral.run().get == ".231f")
      assert(new ScalaSyntax(".231F").FloatingPointLiteral.run().get == ".231F")
      assert(new ScalaSyntax(".231d").FloatingPointLiteral.run().get == ".231d")
      assert(new ScalaSyntax(".231D").FloatingPointLiteral.run().get == ".231D")
      assert(new ScalaSyntax("1e3").FloatingPointLiteral.run().get == "1e3")
      assert(new ScalaSyntax("1e-3").FloatingPointLiteral.run().get == "1e-3")
      assert(new ScalaSyntax("1.3E-3").FloatingPointLiteral.run().get == "1.3E-3")
      assert(new ScalaSyntax("1.3E-3F").FloatingPointLiteral.run().get == "1.3E-3F")
    }

    it("should work for symbols") {
      assert(new ScalaSyntax("'Sym").SymbolLiteral.run().get == "Sym")
      assert(new ScalaSyntax("'sym").SymbolLiteral.run().get == "sym")
      assert(new ScalaSyntax("'*").SymbolLiteral.run().get == "*")
    }

    it("matches any literal when using the Literal rule") {
      assert(new ScalaSyntax("34").Literal.run().get == "34")
      assert(new ScalaSyntax("-34").Literal.run().get == "-34")
      assert(new ScalaSyntax("34e3").Literal.run().get == "34e3")
      assert(new ScalaSyntax("-34e3").Literal.run().get == "-34e3")
      assert(new ScalaSyntax("'Sym").Literal.run().get == "Sym")
      assert(new ScalaSyntax("false").Literal.run().get == "false")
      assert(new ScalaSyntax("null").Literal.run().get == "null")
    }
  }

}
