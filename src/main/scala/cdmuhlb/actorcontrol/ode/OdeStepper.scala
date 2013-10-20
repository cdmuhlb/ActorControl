package cdmuhlb.actorcontrol.ode

trait OdeStep[Y <: OdeYState[Y, D], D <: OdeDyDtState[D, Y]] {
  def startTime: Double
  def endTime: Double
  def interpolate(t: Double): OdeState[Y, D]

  def containsTime(t: Double): Boolean = (t >= startTime) && (t <= endTime)
}

trait OdeStepper[S <: OdeStep[Y, D], O <: Ode[Y, D],
    Y <: OdeYState[Y, D], D <: OdeDyDtState[D, Y]] {
  def step(state: OdeState[Y, D], t: Double): S
  def step(seg: S, t: Double): S
}

trait ErrorMeasurer[Y <: OdeYState[Y, D], D <: OdeDyDtState[D, Y]] {
  def measureError(y1: OdeState[Y, D], y2: OdeState[Y, D]): Double
}

trait EmbeddedOdeStepper[S <: OdeStep[Y, D], O <: Ode[Y, D],
    Y <: OdeYState[Y, D], D <: OdeDyDtState[D, Y]]
    extends OdeStepper[S, O, Y, D] {
  def step(state: OdeState[Y, D], t: Double, measurer: ErrorMeasurer[Y, D]):
      (S, Double)
  def step(seg: S, t: Double, measurer: ErrorMeasurer[Y, D]):
      (S, Double)
}
