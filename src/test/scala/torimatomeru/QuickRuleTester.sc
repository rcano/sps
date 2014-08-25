package torimatomeru

object QuickRuleTester {
  new ScalaSyntax(s"""finally """).Id.run()       //> res0: <error> = Success(())
}