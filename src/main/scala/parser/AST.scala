package parser

import typecheck.Type

case class Program(statements : Seq[Statement])

sealed trait Statement
  case class VarAssignmentAndDeclaration(varname: String, expression: Expression, loc : Int) extends Statement
  case class VarAssignment(varname: String, expression: Expression, loc : Int) extends Statement
  case class SubroutineCall(fcall : FunctionCall, loc : Int) extends Statement
  case class IfThenElse(_condition : Expression, _then: Seq[Statement], _else : Seq[Statement], loc : Int) extends Statement
  case class FunctionDeclaration(name : String, parameters : Seq[TypeBinding], body: Seq[Statement], returnType:Type, loc : Int) extends Statement
  case class TypeBinding(varname: String, _type: Type) extends Statement
  case class Return(varname : String, loc : Int) extends Statement
  case class WhileLoop(_condition : Expression, body : Seq[Statement], loc : Int) extends Statement

sealed trait PrimitiveValue
  case class PInt(value: Integer) extends PrimitiveValue
  case class PFloat(value: Double) extends PrimitiveValue
  case class PString(value: String) extends PrimitiveValue
  case class PBool(value : Boolean) extends PrimitiveValue

sealed trait Expression
  case class Expr(operator: BinaryOperator, l_operand: Expression, r_operand : Expression, loc : Int) extends Expression
  case class Identifier(value : String, loc : Int) extends Expression
  case class ExprValue(value : PrimitiveValue, loc : Int) extends Expression
  case class FunctionCall(name : String, parameters : Seq[Expression], loc: Int) extends Expression

sealed trait BinaryOperator
  class BooleanOperator() extends BinaryOperator
  class ArithmeticOperator() extends BinaryOperator

  case class Plus() extends ArithmeticOperator
  case class Minus() extends ArithmeticOperator
  case class Mult() extends ArithmeticOperator
  case class Div() extends ArithmeticOperator
  case class Mod() extends ArithmeticOperator
  case class Gt() extends BooleanOperator
  case class Lt() extends BooleanOperator
  case class Eq() extends BooleanOperator
  case class Neq() extends BooleanOperator