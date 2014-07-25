sps
===

Scala parboiled2 syntax is a project which simply defines all of the syntax of scala in parboiled2 rules.
Note that the project doesn't aim to provide an AST at all.

The idea of the project is to have a set of rules that correctly represent the current state of scala's syntax without defining any AST,
so in order you use it, you would fork it and add your own ast to the already defined rules.

Current State
-------------

I just finished defning all the rules from the spec but in an almost automatic way, it probably wont work since the rules in the spec don't mind LL parsers at all (the literals do work though).
