package remilb


package object untypedlambdacalculus {
  /** Returns a normalized Nameless Lambda expression using call-by-value semantics from Pierce 5-3 */
  def normalizeExpression(expr: NLE): NLE = expr match {
    //E-AppAbs
    case NApply(NLambda(e1), NLambda(e2)) => normalizeExpression(E_AppAbs(NLambda(e1), NLambda(e2)))
    //E-App2
    case NApply(NLambda(e1), e2 @ NApply(t1, t2)) => normalizeExpression(NApply(NLambda(e1), normalizeExpression(e2)))
    //E-App1
    case NApply(e1 @ NApply(t1, t2), e2 @ NApply(t3, t4)) => normalizeExpression(NApply(normalizeExpression(e1), e2))
    //Return normal form terms
    case _ => expr
  }

  /** Eval rule for E-AppAbs as per Pierce 6.3, helper for normalizeExpression */
  def E_AppAbs(v1: NLambda, v2: NLambda): NLE = {
    val temp = substitute(shift(v2, 1, 0), NVar(0), v1.e)
    shift(temp, -1, 0)
  }

  /** Takes a Named Lambda Expression (LambdaExpression) and converts to Nameless Lambda Expression (NLE), Charlie's code from Piazza */
  def removeNames(gamma:List[Char], expr:LambdaExpression):NLE = expr match {
    case Var(x) => NVar(gamma.indexOf(x))
    case Lambda(Var(x), e) => NLambda(removeNames(x::gamma, e))
    case Apply(e1, e2) => NApply(removeNames(gamma,e1), removeNames(gamma, e2))
  }

  /** Takes a nameless term and naming context and restores names as per Pierce */
  def restoreNames(namelessTerm: NLE, namingContext: List[Char]): LambdaExpression = namelessTerm match {
    case NVar(i) => Var(namingContext(i))
    case NLambda(e) => Lambda(Var(getFreshName(namingContext)), restoreNames(e, getFreshName(namingContext)::namingContext))
    case NApply(t1, t2) => Apply(restoreNames(t1, namingContext), restoreNames(t2, namingContext))
  }

  /** Helper function for restoreNames, returns a fresh variable name to restore bound vars */
  def getFreshName(namingContext: List[Char]): Char = {
    val possibleNames = ('a' to 'z').toSet
    val freshNames = possibleNames diff namingContext.toSet
    freshNames.head
  }

  /** Returns set of the free variables in Lambda expression [term], as per Pierce 5.3.2 */
  def getFreeVariables(term: LambdaExpression): Set[Char] = term match {
    case Var(x) => Set(x)
    case Lambda(bound_var, t1) => getFreeVariables(t1) - bound_var.x
    case Apply(t1, t2) => getFreeVariables(t1) union getFreeVariables(t2)
  }

  /**
   * Substitution function for nameless terms as per Pierce 6.2.4
   * @param term Nameless Lambda term to substitute
   * @param var_num Variable that is being replaced by substitution
   * @param expr Nameless Lambda expression to substitute into
   * @return NLE of the post-substitution term
   */
  def substitute(term:NLE, var_num:NLE, expr:NLE):NLE = expr match {
    case NVar(_) => if (var_num == expr) term else expr
    case NLambda(t1) => NLambda(substitute(shift(term, 1, 0), shift(var_num, 1, 0), t1))
    case NApply(t1, t2) => NApply(substitute(term, var_num, t1), substitute(term, var_num, t2))
  }

  /** Shift function as per Pierce 6.2.1 */
  def shift(term:NLE, d:Int, cutoff:Int):NLE = term match {
    case NVar(i) =>  if (i < cutoff) NVar(i) else NVar(i+d)
    case NLambda(e) => NLambda(shift(e, d, cutoff+1))
    case NApply(t1, t2) => NApply(shift(t1, d, cutoff), shift(t2, d , cutoff))
  }

  /**
   * Performs all necessary operations to evaluate a Lambda Expression with named variables
   * @param input Lambda expression to normalize
   * @return Normalized Lambda expression
   */
  def evaluateLambdaExpression(input: LambdaExpression): LambdaExpression = {
    val freeVars = getFreeVariables(input)
    val namingContext = freeVars.toList
    val namelessExpr = removeNames(namingContext, input)
    val normalizedExpr = normalizeExpression(namelessExpr)
    restoreNames(normalizedExpr, namingContext)
  }
}
