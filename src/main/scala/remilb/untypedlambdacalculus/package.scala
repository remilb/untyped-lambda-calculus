package remilb

package object untypedlambdacalculus {

  /** Returns a normalized Nameless Lambda expression using call-by-value semantics from Pierce 5-3 */
  def normalizeExpression(expr: NLE): NLE = expr match {
    //E-AppAbs
    case NApplication(NAbstraction(e1), NAbstraction(e2)) =>
      normalizeExpression(E_AppAbs(NAbstraction(e1), NAbstraction(e2)))
    //E-App2
    case NApplication(NAbstraction(e1), e2 @ NApplication(t1, t2)) =>
      normalizeExpression(
        NApplication(NAbstraction(e1), normalizeExpression(e2))
      )
    case NApplication(e1 @ NApplication(t1, t2), abs @ NAbstraction(e2)) =>
      normalizeExpression(NApplication(normalizeExpression(e1), abs))
    //E-App1
    case NApplication(e1 @ NApplication(t1, t2), e2 @ NApplication(t3, t4)) =>
      normalizeExpression(NApplication(normalizeExpression(e1), e2))
    //Return normal form terms
    case _ => expr
  }

  /** Eval rule for E-AppAbs as per Pierce 6.3, helper for normalizeExpression */
  def E_AppAbs(v1: NAbstraction, v2: NAbstraction): NLE = {
    val temp = substitute(shift(v2, 1, 0), NVar(0), v1.e)
    shift(temp, -1, 0)
  }

  /** Takes a Named Lambda Expression (LambdaExpression) and converts to Nameless Lambda Expression (NLE) */
  def removeNames(gamma: List[Char], expr: LambdaExpression): NLE = expr match {
    case Var(x)                 => NVar(gamma.indexOf(x))
    case Abstraction(Var(x), e) => NAbstraction(removeNames(x :: gamma, e))
    case Application(e1, e2) =>
      NApplication(removeNames(gamma, e1), removeNames(gamma, e2))
  }

  /** Takes a nameless term and naming context and restores names as per Pierce */
  def restoreNames(namelessTerm: NLE,
                   namingContext: List[Char]): LambdaExpression =
    namelessTerm match {
      case NVar(i) => Var(namingContext(i))
      case NAbstraction(e) =>
        Abstraction(
          Var(getFreshName(namingContext)),
          restoreNames(e, getFreshName(namingContext) :: namingContext)
        )
      case NApplication(t1, t2) =>
        Application(
          restoreNames(t1, namingContext),
          restoreNames(t2, namingContext)
        )
    }

  /** Helper function for restoreNames, returns a fresh variable name to restore bound vars */
  def getFreshName(namingContext: List[Char]): Char = {
    val possibleNames = ('a' to 'z').toSet
    val freshNames = possibleNames diff namingContext.toSet
    freshNames.head
  }

  /** Returns set of the free variables in Lambda expression [term], as per Pierce 5.3.2 */
  def getFreeVariables(term: LambdaExpression): Set[Char] = term match {
    case Var(x)                     => Set(x)
    case Abstraction(bound_var, t1) => getFreeVariables(t1) - bound_var.x
    case Application(t1, t2)        => getFreeVariables(t1) union getFreeVariables(t2)
  }

  /**
    * Substitution function for nameless terms as per Pierce 6.2.4
    * @param term Nameless Lambda term to substitute
    * @param var_num Variable that is being replaced by substitution
    * @param expr Nameless Lambda expression to substitute into
    * @return NLE of the post-substitution term
    */
  def substitute(term: NLE, var_num: NLE, expr: NLE): NLE = expr match {
    case NVar(_) => if (var_num == expr) term else expr
    case NAbstraction(t1) =>
      NAbstraction(substitute(shift(term, 1, 0), shift(var_num, 1, 0), t1))
    case NApplication(t1, t2) =>
      NApplication(substitute(term, var_num, t1), substitute(term, var_num, t2))
  }

  /** Shift function as per Pierce 6.2.1 */
  def shift(term: NLE, d: Int, cutoff: Int): NLE = term match {
    case NVar(i)         => if (i < cutoff) NVar(i) else NVar(i + d)
    case NAbstraction(e) => NAbstraction(shift(e, d, cutoff + 1))
    case NApplication(t1, t2) =>
      NApplication(shift(t1, d, cutoff), shift(t2, d, cutoff))
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
