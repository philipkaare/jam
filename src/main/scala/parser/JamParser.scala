package parser

import org.parboiled2._
import shapeless.HNil
import typecheck._

import scala.language.postfixOps

class JamParser(val input: ParserInput) extends Parser {

  def StringLiteralToken : Rule1[Expression] = rule {
    (ch('"') ~ capture(oneOrMore(ch(' ')|ch('\'')|ch('?')|ch(':')|ch('(')|ch(')')|ch('!')|CharPredicate.AlphaNum.named("character"))) ~ ch('"') ~> ((s : String) => ExprValue(PString(s),cursor))) ~ Whitespace}
  def Digits : Rule0 = rule { oneOrMore(CharPredicate.Digit) }
  def FloatingDigits : Rule0 = rule { oneOrMore(CharPredicate.Digit) ~ ch('.') ~ oneOrMore(CharPredicate.Digit) }
  def NumberToken : Rule1[Expression] = rule { capture(Digits) ~> ((n : String) => ExprValue(PInt (n.toInt), cursor)) ~ Whitespace }
  def FloatToken : Rule1[Expression] = rule {capture(FloatingDigits)~> ((n : String) => ExprValue(PFloat (n.toDouble),cursor)) ~ Whitespace }
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
  def ParseType : Rule1[Type] = rule {
    capture("Int"|"Float"|"String"|"Bool"|"Unit") ~> ((t : String) =>
    t match {
      case "Int" => TInt()
      case "Float" => TFloat()
      case "String" => TString()
      case "Bool" => TBool()
      case "Unit" => TUnit()
    })}
  def Whitespace : Rule0 = rule { zeroOrMore(' ') }
  def RequiredWhiteSpace : Rule0 = rule {oneOrMore(' ')}
  def NewLine : Rule0 = rule { oneOrMore('\n') | &(EOI) }
  def EatNewLines : Rule0 = rule {zeroOrMore('\n') | &(EOI)}
  def IdentifierToken : Rule1[String] = rule { capture(oneOrMore(CharPredicate.Alpha.named("character"))) ~ Whitespace }

  def KeywordVar: Rule0= rule { atomic("var") ~ RequiredWhiteSpace }
  def KeywordAssignment : Rule0 = rule { atomic("<-") ~ Whitespace }
  def KeywordReturn: Rule0 = rule { atomic("return") ~ RequiredWhiteSpace }
  def ParseAssignmentAndDeclaration : Rule1[Statement] = rule {
    (KeywordVar ~ IdentifierToken ~ KeywordAssignment ~ ParseExpression) ~> ((i : String, e : Expression) => VarAssignmentAndDeclaration(i,e, cursor))
  }
  def ParseReturnStatement : Rule1[Statement] = rule { KeywordReturn ~ IdentifierToken ~> (varname => Return(varname, cursor))}
  def ParseAssignment : Rule1[Statement] = rule {
    (IdentifierToken ~ KeywordAssignment ~ ParseExpression) ~> ((i : String, e : Expression) => VarAssignment(i,e, cursor))
  }

  def ParseIfThenElse : Rule1[IfThenElse] = rule {
    (atomic("if") ~ RequiredWhiteSpace ~ ParseExpression ~ atomic("then") ~ Whitespace  ~ NewLine ~ oneOrMore(Whitespace ~ ParseStatement) ~ atomic("else") ~ NewLine ~ oneOrMore(Whitespace ~ ParseStatement) ~ Whitespace ~ atomic("end if")) ~>
      ((cond : Expression, _then : Seq[Statement], _else : Seq[Statement]) => IfThenElse(cond,_then,_else, cursor))
  }

  def ParseWhileLoop : Rule1[WhileLoop] = rule {
    (atomic("while") ~ RequiredWhiteSpace ~ ParseExpression ~ NewLine ~ oneOrMore(Whitespace ~ ParseStatement) ~ Whitespace ~ atomic("end while")) ~>
      ((cond : Expression, body : Seq[Statement]) => WhileLoop(cond, body, cursor))
  }

  def ParseStatement : Rule1[Statement] = rule { Whitespace ~ (ParseFunctionDeclaration|ParseAssignment | ParseAssignmentAndDeclaration | ParseSubroutineCall | ParseIfThenElse | ParseWhileLoop | ParseReturnStatement) ~ Whitespace ~ NewLine}

  def ParseProgram : Rule1[Program] = rule { oneOrMore(ParseStatement) ~> (ss => Program(ss)) }

  def Input : Rule1[Program] = rule { ParseProgram ~ EOI }

  def ParseSubroutineCall : Rule1[Statement] = rule { ParseFunctionCall ~> ((f : FunctionCall) => SubroutineCall(f, cursor))}

  def ParseTypeBinding : Rule1[TypeBinding] = rule{ (IdentifierToken~Whitespace~':'~Whitespace~ParseType) ~>
    ((name : String, _type : Type) => TypeBinding(name, _type)) }

  def ParametersDeclaration : Rule1[Seq[TypeBinding]] = rule { zeroOrMore(ParseTypeBinding).separatedBy(ch(',')~Whitespace)  }

  def ParseFunctionHeading : Rule2[String,Tuple2[Seq[TypeBinding], Type]] = rule { (atomic("func") ~ RequiredWhiteSpace ~ IdentifierToken ~ Whitespace ~
    ch('(') ~ (ParametersDeclaration ~ch(')')~Whitespace~':' ~Whitespace ~ParseType~NewLine) ~> ((params : Seq[TypeBinding], t : Type) => (params, t)))}

  def ParseFunctionDeclaration : Rule1[Statement] = rule {(ParseFunctionHeading ~ Whitespace ~ oneOrMore(ParseStatement) ~ atomic("end func") ~ Whitespace) ~>
    ((fname : String, t: Tuple2[Seq[TypeBinding], Type] , stats : Seq[Statement]) => t match {
        case (params : Seq[TypeBinding], returnType : Type) => FunctionDeclaration(fname, params, stats,returnType, cursor)
      })}

  def ParseFunctionCall : Rule1[FunctionCall] = rule {
    (IdentifierToken ~ ch('(') ~ zeroOrMore(ParseExpression).separatedBy(ch(',')~Whitespace) ~ ch(')') ) ~>
      ((name, params) => FunctionCall(name, params, cursor))
  }

  def ParseExpression : Rule1[Expression]  = rule {
    Term ~ zeroOrMore(AdditiveToken ~ Term  ~>
        ((l : Expression, op : BinaryOperator, r : Expression) => Expr(op, l, r, cursor))
    )
  }

  def Term : Rule1[Expression] = rule {
    Factor ~ zeroOrMore(MultiplicativeToken ~ Factor ~>
      ((l : Expression, op : BinaryOperator, r : Expression) => Expr(op, l, r, cursor))
    )
  }

  def Factor: Rule1[Expression] = rule {
    StringLiteralToken |
      FloatToken | NumberToken | ParseFunctionCall |ch('(') ~ ParseExpression ~ ch(')') |
      IdentifierToken ~> (v => Identifier(v, cursor) )}
}
