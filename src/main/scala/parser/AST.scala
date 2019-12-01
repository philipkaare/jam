package parser

case class Program(statements : Seq[Statement])

sealed trait Statement
  case class VarAssignment(varname: String, expression: Expression) extends Statement
  case class SubroutineCall(fcall : FunctionCall) extends Statement

sealed trait PrimitiveValue
  case class PInt(value: Integer) extends PrimitiveValue
  case class PDouble(value: Double) extends PrimitiveValue
  case class PString(value: String) extends PrimitiveValue

sealed trait Expression
  case class Expr(operator: Operator, l_operand: Expression, r_operand : Expression) extends Expression
  case class Identifier(value : String) extends Expression
  case class ExprValue(value : PrimitiveValue) extends Expression
  case class FunctionCall(name : String, parameters : Seq[Expression]) extends Expression

sealed trait Operator
  case class Plus() extends Operator
  case class Minus() extends Operator
  case class Mult() extends Operator
  case class Div() extends Operator
  case class Mod() extends Operator
