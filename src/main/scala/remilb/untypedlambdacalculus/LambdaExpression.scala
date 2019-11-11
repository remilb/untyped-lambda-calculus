package remilb.untypedlambdacalculus

sealed abstract class LambdaExpression

case class Var(x: Char) extends LambdaExpression {
  override def toString = x.toString
}

case class Lambda(x: Var, e: LambdaExpression) extends LambdaExpression {
  def apply(x: LambdaExpression) = Apply(this, x)
  override def toString = "\\" + x + " -> " + e
}

case class Apply(e1: LambdaExpression, e2: LambdaExpression) extends LambdaExpression {
  override def toString = "(" + e1.toString + " " + e2.toString + ")"
}

/** Case classes for Nameless remilb.untypedlambdacalculus.Lambda Expressions*/
sealed abstract class NLE

case class NVar(x: Int) extends NLE {
  override def toString = x.toString
}

case class NLambda(e: NLE) extends NLE {
  override def toString = "\\" + "." + e
}

case class NApply(e1: NLE, e2: NLE) extends NLE {
  override def toString = "(" + e1.toString + " " + e2.toString + ")"
}
