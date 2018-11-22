package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal(
      b()*b() - 4*a()*c()
    )
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal(
      delta() match {
        case n if n < 0 => Set()
        case 0 => Set((-b()) / (2*a()))
        case n => Set(
          (-b() + math.sqrt(n)) / (2*a()),
          (-b() - math.sqrt(n)) / (2*a())
        )
      }
    )
  }
}
