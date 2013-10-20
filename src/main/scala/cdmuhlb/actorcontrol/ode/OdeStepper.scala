package cdmuhlb.actorcontrol.ode

trait OdeStep[Y <: OdeYState[Y, D], D <: OdeDyDtState[D, Y]] {
  def startTime: Double
  def endTime: Double
  def endState: OdeState[Y, D]
  def interpolate(t: Double): OdeState[Y, D]

  def stepSize: Double = endTime - startTime
  def containsTime(t: Double): Boolean = (t >= startTime) && (t <= endTime)
}

trait OdeStepper[S <: OdeStep[Y, D], O <: Ode[Y, D],
    Y <: OdeYState[Y, D], D <: OdeDyDtState[D, Y]] {
  def step(state: OdeState[Y, D], t: Double): S
  def step(seg: S, t: Double): S
}

trait EmbeddedOdeStepper[S <: OdeStep[Y, D], O <: Ode[Y, D],
    Y <: OdeYState[Y, D], D <: OdeDyDtState[D, Y]]
    extends OdeStepper[S, O, Y, D] {
  def embeddedStep(state: OdeState[Y, D], t: Double): (S, OdeState[Y, D])
  def embeddedStep(lastStep: S, t: Double): (S, OdeState[Y, D])
}
