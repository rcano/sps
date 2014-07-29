package torimatomeru

import language.implicitConversions
import syntax._
import org.parboiled2._

class ScalaSyntax(val input: ParserInput) extends Parser with Basic with Identifiers with Literals {

  def Whitespace = rule { zeroOrMore(WhitespaceChar) | Comment }
  
  /**
   * Every token handles space at the end.
   * Don't let it propagate to mixins
   */
  implicit private[this] def wspStr(s: String): Rule0 = rule {
    str(s) ~ Whitespace
  }
  implicit private[this] def wspChar(s: Char): Rule0 = rule {
    ch(s) ~ Whitespace
  }

  //////////////////////////////////////////////////
  // Override rules from dependencies
  // in order to handle white spaces
  // Note: when you add your AST, make sure to
  // only capture super.rule and not the whitespace
  //////////////////////////////////////////////////
  
  override def Id = rule { super.Id ~ Whitespace }
  override def VarId = rule { super.VarId ~ Whitespace }
  override def Literal = rule { super.Literal ~ Whitespace }
  
  ///////////////////////////////////////////
  // Qualifiers and Ids
  ///////////////////////////////////////////
  
  def QualId = rule { oneOrMore(Id) separatedBy '.' }
  def Ids = rule { oneOrMore(Id) separatedBy ',' }

  //path and stableId were refactored (wrt spec) to avoid recursiveness and be more specific 
  def Path: Rule0 = rule { zeroOrMore(Id ~ '.') ~ "this" ~ zeroOrMore(Id).separatedBy('.') | StableId }
  def StableId: Rule0 = rule { zeroOrMore(Id ~ '.') ~ optional(("this" | "super" ~ optional(ClassQualifier)) ~ oneOrMore(Id).separatedBy('.')) }
  def ClassQualifier = rule { '[' ~ Id ~ ']' }

  ///////////////////////////////////////////
  // Types and more Types
  ///////////////////////////////////////////

  def Type: Rule0 = rule { FunctionArgTypes ~ "=>" ~ Type | InfixType ~ optional(ExistentialClause) }
  def FunctionArgTypes = rule { InfixType | '(' ~ optional(oneOrMore(ParamType) separatedBy ',') ~ ')' }

  def ExistentialClause = rule { "forSome" ~ '{' ~ oneOrMore(ExistentialDcl).separatedBy(Semi) }
  def ExistentialDcl = rule { "type" ~ TypeDcl | "val" ~ ValDcl }

  def InfixType = rule { CompoundType ~ zeroOrMore(Id ~ optional(Newline) ~ CompoundType) }
  def CompoundType = rule { oneOrMore(AnnotType).separatedBy("with") ~ optional(Refinement) }
  def AnnotType = rule { SimpleType ~ zeroOrMore(Annotation) }
  def SimpleType: Rule0 = rule { '(' ~ Types ~ ')' | Path ~ '.' ~ "type" | StableId | SimpleType ~ '#' ~ Id | SimpleType ~ TypeArgs }
  def TypeArgs = rule { '[' ~ Types ~ ']' }
  def Types = rule { oneOrMore(Type).separatedBy(',') }
  def Refinement = rule { optional(Newline) ~ '{' ~ oneOrMore(RefineStat).separatedBy(Semi) ~ '}' }
  def RefineStat = rule { "type" ~ TypeDef | Dcl | MATCH }
  def TypePat = rule { Type }
  def Ascription = rule { ":" ~ (InfixType | oneOrMore(Annotation) | "_" ~ "*") }

  def ParamType = rule { "=>" ~ Type | Type ~ "*" | Type }

  /////////////////////////////////////////////////
  // Declarations, Expressions and Pattern Matching
  /////////////////////////////////////////////////

  def Expr: Rule0 = rule { (Bindings | optional("implicit") ~ Id | "_") ~ "=>" ~ Expr | Expr1 }
  def Expr1: Rule0 = rule {
    IfCFlow |
      WhileCFlow |
      TryCFlow |
      DoWhileCFlow |
      ForCFlow |
      "throw" ~ Expr |
      "return" ~ optional(Expr) |
      optional(SimpleExpr ~ '.') ~ Id ~ '=' ~ Expr |
      SimpleExpr1 ~ ArgumentExprs ~ '=' ~ Expr |
      PostfixExpr ~ Ascription |
      PostfixExpr ~ "match" ~ '{' ~ CaseClauses ~ '}' |
      PostfixExpr
  }
  def IfCFlow = rule { "if" ~ '(' ~ Expr ~ ')' ~ zeroOrMore(Newline) ~ Expr ~ optional(optional(Semi) ~ "else" ~ Expr) }
  def WhileCFlow = rule { "while" ~ '(' ~ Expr ~ ')' ~ zeroOrMore(Newline) ~ Expr }
  def TryCFlow = rule { "try" ~ '{' ~ Block ~ '}' ~ optional("catch" ~ '{' ~ CaseClauses ~ '}') ~ optional("finally" ~ Expr) }
  def DoWhileCFlow = rule { "do" ~ Expr ~ optional(Semi) ~ "while" ~ '(' ~ Expr ~ ')' }
  def ForCFlow = rule { "for" ~ ('(' ~ Enumerators ~ ')' | '{' ~ Enumerators ~ '}') ~ zeroOrMore(Newline) ~ optional("yield") ~ Expr }
  def PostfixExpr: Rule0 = rule { InfixExpr ~ optional(Id ~ optional(Newline)) }
  def InfixExpr: Rule0 = rule { PrefixExpr | InfixExpr ~ Id ~ optional(Newline) ~ InfixExpr }
  def PrefixExpr = rule { optional(anyOf("-+~!")) ~ SimpleExpr }
  def SimpleExpr: Rule0 = rule { "new" ~ (ClassTemplate | TemplateBody) | BlockExpr | SimpleExpr1 ~ optional('_') }
  def SimpleExpr1: Rule0 = rule {
    Literal ~ drop[String] | //literal currently captures, so it can be used outside. but since all our rules lack AST, we drop its value in order to be able to compose them
      Path |
      '_' |
      '(' ~ optional(Exprs) ~ ')' |
      SimpleExpr ~ '.' ~ Id |
      SimpleExpr ~ TypeArgs |
      SimpleExpr1 ~ ArgumentExprs /*| //this line doesn't make sense, if none of the previous options work, there is no way that a recursion on SimpleExpr1 will work
    XmlExpr*/
  }
  def Exprs: Rule0 = rule { oneOrMore(Expr) separatedBy ',' }
  def ArgumentExprs: Rule0 = rule {
    '(' ~ (optional(Exprs ~ ',') ~ PostfixExpr ~ ':' ~ '_' ~ '*' | optional(Exprs)) ~ ')' |
      optional(Newline) ~ BlockExpr
  }
  def BlockExpr: Rule0 = rule { '{' ~ (CaseClauses | Block) ~ '}' }
  def Block: Rule0 = rule { zeroOrMore(BlockStat ~ Semi) ~ optional(ResultExpr) }
  def BlockStat: Rule0 = rule {
    Import |
      zeroOrMore(Annotation) ~ (optional("implicit" | "lazy") ~ Def | zeroOrMore(LocalModifier) ~ TmplDef) |
      Expr1 |
      MATCH
  }
  def ResultExpr: Rule0 = rule { (Bindings | optional("implicit") ~ Id | "_") ~ "=>" ~ Block | Expr1 }
  def Enumerators: Rule0 = rule { Generator ~ zeroOrMore(Semi ~ Enumerator) }
  def Enumerator: Rule0 = rule { Generator | Guard | Pattern1 ~ '=' ~ Expr }
  def Generator: Rule0 = rule { Pattern1 ~ "<-" ~ Expr ~ optional(Guard) }
  def CaseClauses: Rule0 = rule { oneOrMore(CaseClause) }
  def CaseClause: Rule0 = rule { "case" ~ Pattern ~ optional(Guard) ~ "=>" ~ Block }
  def Guard: Rule0 = rule { "if" ~ PostfixExpr }
  def Pattern: Rule0 = rule { oneOrMore(Pattern) separatedBy '|' }
  def Pattern1: Rule0 = rule { '_' ~ ':' ~ TypePat | VarId ~ ':' ~ TypePat | Pattern2 }
  def Pattern2: Rule0 = rule { VarId ~ optional("@" ~ Pattern3) }
  def Pattern3: Rule0 = rule { SimplePattern ~ zeroOrMore(Id ~ optional(Newline) ~ SimplePattern) | SimplePattern } // this pattern doesn't make sense to me...
  def SimplePattern: Rule0 = rule {
    '_' |
      VarId |
      Literal ~ drop[String] | //literal currently captures, so it can be used outside. but since all our rules lack AST, we drop its value in order to be able to compose them
      StableId ~ '(' ~ (optional(Patterns ~ ',') ~ optional(VarId ~ '@') ~ '_' ~ '*' | optional(Patterns)) ~ ')' |
      '(' ~ optional(Patterns) ~ ')' /*|
    XmlPattern*/
  }
  def Patterns: Rule0 = rule { '_' ~ '*' | oneOrMore(Pattern).separatedBy(',') }

  def TypeParamClause: Rule0 = rule { '[' ~ oneOrMore(VariantTypeParam).separatedBy(',') ~ ']' }
  def FunTypeParamClause: Rule0 = rule { '[' ~ oneOrMore(TypeParam).separatedBy(',') ~ ']' }
  def VariantTypeParam: Rule0 = rule { zeroOrMore(Annotation) ~ optional(anyOf("+-")) ~ TypeParam }
  def TypeParam: Rule0 = rule { (Id | '_') ~ optional(TypeParamClause) ~ optional(">:" ~ Type) ~ optional("<:" ~ Type) ~ zeroOrMore("<%" ~ Type) ~ zeroOrMore(':' ~ Type) }
  def ParamClauses: Rule0 = rule { zeroOrMore(ParamClause) ~ optional(optional(Newline) ~ '(' ~ "implicit" ~ Params ~ ')') }
  def ParamClause: Rule0 = rule { optional(Newline) ~ '(' ~ optional(Params) ~ ')' }
  def Params: Rule0 = rule { zeroOrMore(Param).separatedBy(',') }
  def Param: Rule0 = rule { zeroOrMore(Annotation) ~ Id ~ optional(':' ~ ParamType) ~ optional('=' ~ Expr) }
  def ClassParamClauses: Rule0 = rule { zeroOrMore(ClassParamClause) ~ optional(optional(Newline) ~ '(' ~ "implicit" ~ ClassParam ~ ')') }
  def ClassParamClause: Rule0 = rule { optional(Newline) ~ '(' ~ optional(ClassParam) ~ ')' }
  def ClassParams: Rule0 = rule { oneOrMore(ClassParam).separatedBy(',') }
  def ClassParam: Rule0 = rule { zeroOrMore(Annotation) ~ optional(zeroOrMore(Modifier) ~ ("val" | "var")) ~ Id ~ ":" ~ ParamType ~ optional("=" ~ Expr) }

  def Bindings: Rule0 = rule { '(' ~ oneOrMore(Binding).separatedBy(',') ~ ')' }
  def Binding: Rule0 = rule { (Id | '_') ~ optional(':' ~ Type) }

  def Modifier: Rule0 = rule { LocalModifier | AccessModifier | "override" }
  def LocalModifier: Rule0 = rule { "abstract" | "final" | "sealed" | "implicit" | "lazy" }
  def AccessModifier: Rule0 = rule { ("private" | "protected") ~ optional(AccessQualifier) }
  def AccessQualifier: Rule0 = rule { '[' ~ ("this" ~ Id) ~ ']' }

  def Annotation: Rule0 = rule { '@' ~ SimpleType ~ zeroOrMore(ArgumentExprs) }
  def ConstrAnnotation: Rule0 = rule { '@' ~ SimpleType ~ ArgumentExprs }
  def NameValuePair: Rule0 = rule { "val" ~ Id ~ '=' ~ PrefixExpr }

  def TemplateBody: Rule0 = rule { optional(Newline) ~ '{' ~ optional(SelfType) ~ TemplateStat ~ zeroOrMore(Semi ~ TemplateStat) ~ '}' }
  def TemplateStat: Rule0 = rule {
    Import |
      zeroOrMore(Annotation ~ optional(Newline)) ~ zeroOrMore(Modifier) ~ (Def | Dcl) |
      Expr |
      MATCH
  }

  def SelfType: Rule0 = rule { "this" ~ ':' ~ Type ~ "=>" | Id ~ optional(':' ~ Type) ~ "=>" }

  def Import: Rule0 = rule { "import" ~ oneOrMore(ImportExpr).separatedBy(',') }
  
  //ImportExpr is slightly changed wrt spec because StableId always consumes all the Ids possible, so there is no need to one at the end
  def ImportExpr: Rule0 = rule { StableId ~ optional('_' | ImportSelectors) }
  def ImportSelectors: Rule0 = rule { '{' ~ zeroOrMore(ImportSelector ~ ',') ~ (ImportSelector | '_') ~ '}' }
  def ImportSelector: Rule0 = rule { Id ~ optional("=>" ~ (Id | '_')) }

  def Dcl: Rule0 = rule {
    "val" ~ ValDcl |
      "var" ~ VarDcl |
      "def" ~ FunDcl |
      "type" ~ zeroOrMore(Newline) ~ TypeDcl
  }
  def ValDcl: Rule0 = rule { Ids ~ ':' ~ Type }
  def VarDcl: Rule0 = rule { Ids ~ ':' ~ Type }
  def FunDcl: Rule0 = rule { FunSig ~ optional(':' ~ Type) }
  def FunSig: Rule0 = rule { Id ~ optional(FunTypeParamClause) ~ ParamClauses }
  def TypeDcl: Rule0 = rule { Id ~ optional(TypeParamClause) ~ optional(">:" ~ Type) ~ optional("<:" ~ Type) }

  def PatVarDef: Rule0 = rule { "val" ~ PatDef | "var" ~ VarDef }
  def Def: Rule0 = rule { "def" ~ FunDef | "type" ~ zeroOrMore(Newline) ~ TypeDef | PatVarDef | TmplDef }
  def PatDef: Rule0 = rule { oneOrMore(Pattern2).separatedBy(',') ~ optional(':' ~ Type) ~ '=' ~ Expr }
  def VarDef: Rule0 = rule { Ids ~ ':' ~ Type ~ '=' ~ '_' | PatDef }
  def FunDef: Rule0 = rule {
    "this" ~ ParamClause ~ ParamClauses ~ ('=' ~ ConstrExpr | optional(Newline) ~ ConstrBlock) |
      FunSig ~ (optional(':' ~ Type) ~ '=' ~ Expr | optional(Newline) ~ '{' ~ Block ~ '}')
  }
  def TypeDef: Rule0 = rule { Id ~ optional(TypeParamClause) ~ '=' ~ Type }

  def TmplDef: Rule0 = rule { "trait" ~ TraitDef | optional("case") ~ ("class" ~ ClassDef | "object" ~ ObjectDef) }
  def ClassDef: Rule0 = rule { Id ~ optional(TypeParamClause) ~ zeroOrMore(ConstrAnnotation) ~ optional(AccessModifier) ~ ClassParamClauses ~ ClassTemplateOpt }
  def TraitDef: Rule0 = rule { Id ~ optional(TypeParamClause) ~ TraitTemplateOpt }
  def ObjectDef: Rule0 = rule { Id ~ ClassTemplateOpt }
  def ClassTemplateOpt: Rule0 = rule { "extends" ~ ClassTemplate | optional(optional("extends") ~ TemplateBody) }
  def TraitTemplateOpt: Rule0 = rule { "extends" ~ TraitTemplate | optional(optional("extends") ~ TemplateBody) }
  def ClassTemplate: Rule0 = rule { optional(EarlyDefs) ~ ClassParents ~ optional(TemplateBody) }
  def TraitTemplate: Rule0 = rule { optional(EarlyDefs) ~ TraitParents ~ optional(TemplateBody) }
  def ClassParents: Rule0 = rule { Constr ~ zeroOrMore("with" ~ AnnotType) }
  def TraitParents: Rule0 = rule { AnnotType ~ zeroOrMore("with" ~ AnnotType) }
  def Constr: Rule0 = rule { AnnotType ~ zeroOrMore(ArgumentExprs) }
  def EarlyDefs: Rule0 = rule { '{' ~ optional(oneOrMore(EarlyDef).separatedBy(Semi)) ~ '}' ~ "with" }
  def EarlyDef: Rule0 = rule { zeroOrMore(Annotation ~ optional(Newline)) ~ zeroOrMore(Modifier) ~ PatVarDef }
  def ConstrExpr: Rule0 = rule { ConstrBlock | SelfInvocation }
  def ConstrBlock: Rule0 = rule { '{' ~ SelfInvocation ~ zeroOrMore(Semi ~ BlockStat) ~ '}' }
  def SelfInvocation: Rule0 = rule { "this" ~ oneOrMore(ArgumentExprs) }

  def TopStatSeq: Rule0 = rule { oneOrMore(TopStat).separatedBy(Semi) }
  def TopStat: Rule0 = rule { Packaging | PackageObject | Import | zeroOrMore(Annotation ~ optional(Newline)) ~ zeroOrMore(Modifier) ~ TmplDef | MATCH }
  def Packaging: Rule0 = rule { "package" ~ QualId ~ optional(Newline) ~ '{' ~ TopStatSeq ~ '}' }
  def PackageObject: Rule0 = rule { "package" ~ "object" ~ ObjectDef }
  def CompilationUnit: Rule0 = rule { zeroOrMore("package" ~ QualId ~ Semi) ~ TopStatSeq }
}
