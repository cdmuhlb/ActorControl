package cdmuhlb.actorcontrol.ode

trait OdeSolution[S <: OdeStep[Y, D], Y <: OdeYState[Y, D],
    D <: OdeDyDtState[D, Y]] {
  def containsTime(t: Double): Boolean
  def interpolate(t: Double): OdeState[Y, D]
}

class OdeSolutionBuilder[S <: OdeStep[Y, D], Y <: OdeYState[Y, D],
    D <: OdeDyDtState[D, Y]] {
  private var steps = Vector.empty[S]
  def appendStep(step: S): Unit = {
    assert(step.startTime < step.endTime)
    if (steps.nonEmpty) require(step.startTime == steps.last.endTime)
    steps :+= step
  }
  def toSolution: OdeSolution[S, Y, D] = new OdeSolutionImpl(steps)

  private class OdeSolutionImpl(steps: Vector[S]) extends OdeSolution[S, Y, D] {
    def containsTime(t: Double) = search(t).nonEmpty
    def interpolate(t: Double) = {
      val stepOpt = search(t)
      require(stepOpt.nonEmpty)
      stepOpt.get.interpolate(t)
    }

    private def search(t: Double): Option[S] = {
      def binarySearch(lo: Int, hi: Int): Option[S] = {
        assert(lo <= hi)
        val i = (hi + lo)/2
        val step = steps(i)
        if (step.containsTime(t)) Some(step)
        else if (lo == hi) None
        else if (t < step.startTime) binarySearch(lo, i - 1)
        else binarySearch(i + 1, hi)
      }
      if ((t >= steps.head.startTime) && (t <= steps.last.endTime)) {
        binarySearch(0, steps.length-1)
      } else None
    }
  }
}

trait OdeIntegrator[S <: OdeStep[Y, D], O <: Ode[Y, D], Y <: OdeYState[Y, D],
    D <: OdeDyDtState[D, Y]] {
  def integrate(state: OdeState[Y, D], t: Double): OdeSolution[S, Y, D]
}
