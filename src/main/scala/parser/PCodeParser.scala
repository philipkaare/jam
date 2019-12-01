package parser

import org.parboiled2._
import shapeless.HNil

import scala.language.postfixOps

class PCodeParser (val input: ParserInput) extends Parser {

  def StringLiteralToken : Rule1[Expression] = rule {
    ch('"') ~ capture(oneOrMore(CharPredicate.Alpha.named("character"))) ~ ch('"') ~> ((s : String) => ExprValue(PString(s)))}
  def Digits : Rule0 = rule { oneOrMore(CharPredicate.Digit) ~ Whitespace }
  def NumberToken : Rule1[Expression] = rule { capture(Digits) ~> ((n : String) => ExprValue(PInt (n.toInt))) }
  def PlusToken : Rule1[Plus] = rule {ch('+')~ Whitespace ~ push(Plus())}
  def MinusToken : Rule1[Minus] = rule {ch('-')~ Whitespace ~ push(Minus ())}
  def MultToken : Rule1[Mult] = rule {ch('*')~ Whitespace ~push(Mult())}
  def DivToken : Rule1[Div] = rule { ch('/')~ Whitespace ~ push(Div())}
  def ModToken : Rule1[Mod] = rule { atomic("mod") ~ Whitespace ~ push(Mod())}
  def Whitespace : Rule0 = rule { zeroOrMore(' ') }
  def RequiredWhiteSpace : Rule0 = rule {oneOrMore(' ')}
  def NewLine : Rule0 = rule { oneOrMore('\n') | &(EOI) }
  def IdentifierToken : Rule1[String] = rule { capture(oneOrMore(CharPredicate.Alpha.named("character"))) ~ Whitespace }

  def KeywordVar: Rule0= rule { atomic("var") ~ RequiredWhiteSpace }
  def KeywordAssignment : Rule0 = rule { atomic("<-") ~ Whitespace }

  def ParseAssignment : Rule1[Statement] = rule { (
    KeywordVar ~ IdentifierToken ~ KeywordAssignment ~ ParseExpression) ~> ((i : String, e : Expression) => VarAssignment(i,e))
  }

  def ParseStatement : Rule1[Statement] = rule { (ParseAssignment | ParseSubroutineCall) ~ NewLine}

  def ParseProgram : Rule1[Program] = rule { oneOrMore(ParseStatement) ~> (ss => Program(ss)) }

  def Input = rule { ParseProgram ~ EOI }

  def ParseSubroutineCall : Rule1[Statement] = rule { ParseFunctionCall ~> SubroutineCall}

  def ParseFunctionCall : Rule1[FunctionCall] = rule {
    IdentifierToken ~ ch('(') ~ zeroOrMore(ParseExpression).separatedBy(ch(',')) ~ ch(')')~>
      ((name, params) => FunctionCall(name, params) )
  }

  def ParseExpression : Rule1[Expression]  = rule {
    Term ~ zeroOrMore(
      PlusToken ~ Term  ~>
        ((l : Expression, op : Operator, r : Expression) => Expr(op, l, r))
    | MinusToken ~ Term ~> ((l : Expression, op : Operator, r : Expression) => Expr(op, l, r))
    )
  }

  def Term : Rule1[Expression] = rule {
    Factor ~ zeroOrMore(
      MultToken ~ Factor ~> ((l : Expression, op : Operator, r : Expression) => Expr(op, l, r))
    | DivToken ~ Factor ~> ((l : Expression, op : Operator, r : Expression) => Expr(op, l, r))
    | ModToken ~ Factor ~> ((l : Expression, op : Operator, r : Expression) => Expr(op, l, r))
    )
  }

  def Factor: Rule1[Expression] = rule {
    ParseFunctionCall | StringLiteralToken |
    NumberToken | ch('(') ~ ParseExpression ~ ch(')') |
      IdentifierToken ~> Identifier }
}
