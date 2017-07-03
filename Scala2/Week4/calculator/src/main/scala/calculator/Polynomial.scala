package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    // The formula is: Δ = b² - 4ac

    Signal {

      math.pow(b(), 2)  -  (4 * a() * c())

    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {

    // The formula is: (-b ± √Δ) / 2a
    // "Recall that there can be 0 (when Δ is negative)"

    Signal {

      if (delta() < 0) {
        Set.empty // delta is negative -- no roots
      } else {

        Set(
          // (-b + √Δ) / 2a
          (-b() + math.sqrt(delta()))  /  (2 * a()),

          // (-b - √Δ) / 2a
          (-b() - math.sqrt(delta()))  /  (2 * a())
        )
      }
    }
  }
}
