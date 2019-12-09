package typecheck

sealed trait Type
case class TString() extends Type
case class TInt() extends Type
case class TFloat() extends Type
case class TBool() extends Type
case class TUnit() extends Type
case class TNotSet() extends Type
case class TFunctionCall(params : List[Type], returnType : Type) extends Type

object Types {

  def toString(t : Type) : String = {
    t match {
      case TString() => "String"
      case TInt() => "Int"
      case TFloat() => "Float"
      case TBool() => "Boolean"
      case TUnit() => "Unit"
      case TNotSet() => "Unset"
      case TFunctionCall(params, returnType) =>
        "(" + toString(params) + ") => " + toString(returnType)
    }
  }

  def toString(tl : Seq[Type]) : String = "("+ tl.map(t => toString(t)).reduce((l,r)=>l+","+r) + ")"

}
