package parser

import org.parboiled2._
import shapeless.HNil

import scala.language.postfixOps
import scala.reflect.macros.whitebox

class PCodeParser (val input: ParserInput) extends Parser {

  def StringLiteralToken : Rule1[Expression] = rule {
    (ch('"') ~ capture(oneOrMore(ch(' ')|ch(':')|ch('(')|ch(')')|ch('!')|CharPredicate.AlphaNum.named("character"))) ~ ch('"') ~> ((s : String) => ExprValue(PString(s)))) ~ Whitespace}
  def Digits : Rule0 = rule { oneOrMore(CharPredicate.Digit) }
  def FloatingDigits : Rule0 = rule { oneOrMore(CharPredicate.Digit) ~ ch('.') ~ oneOrMore(CharPredicate.Digit) }
  def NumberToken : Rule1[Expression] = rule { capture(Digits) ~> ((n : String) => ExprValue(PInt (n.toInt))) ~ Whitespace }
  def FloatToken : Rule1[Expression] = rule {capture(FloatingDigits)~> ((n : String) => ExprValue(PFloat (n.toDouble))) ~ Whitespace }
  def PlusToken : Rule1[Plus] = rule {ch('+')~ Whitespace ~ push(Plus())}
  def MinusToken : Rule1[Minus] = rule {ch('-')~ Whitespace ~ push(Minus ())}
  def MultToken : Rule1[Mult] = rule {ch('*')~ Whitespace ~push(Mult())}
  def DivToken : Rule1[Div] = rule { ch('/')~ Whitespace ~ push(Div())}
  def GtToken : Rule1[Gt] = rule ('>' ~ Whitespace ~ push(Gt()))
  def LtToken : Rule1[Lt] = rule ('<' ~ Whitespace ~ push(Lt()))
  def EqToken : Rule1[Eq] = rule ('=' ~ Whitespace ~ push(Eq()))
  def NeqToken : Rule1[Neq] = rule {"!=" ~Whitespace ~ push(Neq()) }
  def ModToken : Rule1[Mod] = rule { atomic("mod") ~ Whitespace ~ push(Mod())}
  def AdditiveToken : Rule1[BinaryOperator] = rule { PlusToken | MinusToken | GtToken | LtToken | EqToken | NeqToken}
  def MultiplicativeToken : Rule1[BinaryOperator] = rule { MultToken | DivToken |  ModToken}

  def Whitespace : Rule0 = rule { zeroOrMore(' ') }
  def RequiredWhiteSpace : Rule0 = rule {oneOrMore(' ')}
  def NewLine : Rule0 = rule { oneOrMore('\n') | &(EOI) }
  def IdentifierToken : Rule1[String] = rule { capture(oneOrMore(CharPredicate.Alpha.named("character"))) ~ Whitespace }

  def KeywordVar: Rule0= rule { atomic("var") ~ RequiredWhiteSpace }
  def KeywordAssignment : Rule0 = rule { atomic("<-") ~ Whitespace }

  def ParseAssignment : Rule1[Statement] = rule {
    (KeywordVar ~ IdentifierToken ~ KeywordAssignment ~ ParseExpression) ~> ((i : String, e : Expression) => VarAssignment(i,e))
  }

  def ParseIfThenElse : Rule1[IfThenElse] = rule {
    (atomic("if") ~ RequiredWhiteSpace ~ ParseExpression ~ atomic("then") ~ Whitespace  ~ NewLine ~ oneOrMore(Whitespace ~ ParseStatement) ~ atomic("else") ~ NewLine ~ oneOrMore(Whitespace ~ ParseStatement) ~ Whitespace ~ atomic("end if")) ~>
      ((cond : Expression, _then : Seq[Statement], _else : Seq[Statement]) => IfThenElse(cond,_then,_else))
  }

  def ParseStatement : Rule1[Statement] = rule { (ParseAssignment | ParseSubroutineCall | ParseIfThenElse) ~ NewLine}

  def ParseProgram : Rule1[Program] = rule { oneOrMore(ParseStatement) ~> (ss => Program(ss)) }

  def Input = rule { ParseProgram ~ EOI }

  def ParseSubroutineCall : Rule1[Statement] = rule { ParseFunctionCall ~> SubroutineCall}

  def ParseFunctionCall : Rule1[FunctionCall] = rule {
    IdentifierToken ~ ch('(') ~ zeroOrMore(ParseExpression).separatedBy(ch(',')) ~ ch(')')~>
      ((name, params) => FunctionCall(name, params) )
  }

  def ParseExpression : Rule1[Expression]  = rule {
    Term ~ zeroOrMore(AdditiveToken ~ Term  ~>
        ((l : Expression, op : BinaryOperator, r : Expression) => Expr(op, l, r))
    )
  }

  def Term : Rule1[Expression] = rule {
    Factor ~ zeroOrMore(MultiplicativeToken ~ Factor ~>
      ((l : Expression, op : BinaryOperator, r : Expression) => Expr(op, l, r))
    )
  }

  def Factor: Rule1[Expression] = rule {
    StringLiteralToken |
      FloatToken | NumberToken | ParseFunctionCall |ch('(') ~ ParseExpression ~ ch(')') |
      IdentifierToken ~> Identifier }
}
