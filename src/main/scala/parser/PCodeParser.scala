package parser

import org.parboiled2._

class PCodeParser (val input: ParserInput) extends Parser {
  def Digits = rule { oneOrMore(CharPredicate.Digit) }
  def Number = rule { capture(Digits) ~> (_.toInt) }
  def Whitespace = rule { zeroOrMore(' ') }
  def NewLine = rule { oneOrMore('\n') | &(EOI) }
  def Identifier = rule { capture(zeroOrMore(CharPredicate.Alpha)) }
  def Value = rule { (ch('\"') ~ capture(zeroOrMore(CharPredicate.AlphaNum)) ~ ch('\"') ) | Number | Identifier }

  def KeywordVar = rule { "var" }
  def KeywordAssignment = rule { "<-" }
  def Assignment : Rule2[String, Any] = rule { KeywordVar ~ Whitespace ~ Identifier ~ Whitespace ~ KeywordAssignment ~ Whitespace ~ Value ~ Whitespace ~ NewLine}

  def Input = rule { Assignment ~ EOI }

  def InputLine = rule { Expression ~ EOI }

  def Expression: Rule1[Int] = rule {
    Term ~ zeroOrMore(
      '+' ~ Term ~> ((_: Int) + _)
    | '-' ~ Term ~> ((_: Int) - _))
  }

  def Term = rule {
    Factor ~ zeroOrMore(
      '*' ~ Factor ~> ((_: Int) * _)
    | '/' ~ Factor ~> ((_: Int) / _))
  }

  def Factor = rule { Number | Parens }

  def Parens = rule { '(' ~ Expression ~ ')' }



}
