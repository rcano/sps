package torimatomeru

import org.parboiled2.ParseError

class CommentsSyntaxTest extends BaseTest {

  describe("Comments rules") {
    itShould("match simple comments") {
      ruleSucceeds(s"""//someComment here, even with another // in it""")(_.Comment)
    }
    itShould("match multiline comments") {
      ruleSucceeds(s"""/*
here is another comment
*/""")(_.Comment)
    }
    itShould("match nested multiline comments") {
      ruleSucceeds(s"""/*
here is another /*comment*/ /*and/*a/*very/*nested/*one*/*/*/*/*/
*/""")(_.Comment)
    }
    itShould("match comments right after a Semi") {
      ruleSucceeds(s""";/*some comment*/""")(_.Semi)
    }
  }
}