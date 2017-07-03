package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions.map( {case (name, expr) =>
      val expressionValueCalcSignal : Signal[Double] = Signal {

        val expression : Expr = expr()

        eval(expression, namedExpressions)
      }

      (name -> expressionValueCalcSignal)
    })
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    expr match {
      case Literal(literalValue: Double) => literalValue
      case Ref(nameOfTheReferencedExpression) => {

        val referencedExpr : Expr = getReferenceExpr(nameOfTheReferencedExpression, references)

        /*
        From the Assignment document:
        "Refs to other variables could cause cyclic dependencies
        (e.g., a = b + 1 and b = 2 * a.
        Such cyclic dependencies are considered as errors
        (failing to detect this will cause infinite loops)."

        We prevent such illegal cyclic reference expressions
        from causing an infinite loop by removing the name of
        the current reference from further evaluation: as a result
        such illegal cyclic reference expressions should yield
        Double.NaN (since the cyclic reference will fail to
        de-reference this variable name later on)
         */

        val referencesWithoutThis = references - nameOfTheReferencedExpression

        eval(referencedExpr, referencesWithoutThis)
      }
      case Plus(leftOperand: Expr, rightOperand: Expr) => {
        val leftOperandVale = eval(leftOperand, references)
        val rightOperandVale = eval(rightOperand, references)

        leftOperandVale + rightOperandVale
      }

      case Minus(leftOperand: Expr, rightOperand: Expr) => {
        val leftOperandVale = eval(leftOperand, references)
        val rightOperandVale = eval(rightOperand, references)

        leftOperandVale - rightOperandVale
      }

      case Times(leftOperand: Expr, rightOperand: Expr) => {
        val leftOperandVale = eval(leftOperand, references)
        val rightOperandVale = eval(rightOperand, references)

        leftOperandVale * rightOperandVale
      }

      case Divide(leftOperand: Expr, rightOperand: Expr) => {
        val leftOperandVale = eval(leftOperand, references)
        val rightOperandVale = eval(rightOperand, references)

        leftOperandVale / rightOperandVale
      }
    }
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
