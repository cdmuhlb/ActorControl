package cdmuhlb.actorcontrol.ode

case class LinearStep[Y <: OdeYState[Y, D], D <: OdeDyDtState[D, Y]](
    startState: OdeState[Y, D], endState: OdeState[Y, D])
    extends OdeStep[Y, D] {
  val startTime = startState.t
  val endTime = endState.t
  def interpolate(t: Double) = {
    require(containsTime(t))
    if (t == startTime) startState
    else if (t == endTime) endState
    else {
      val w = (t - startTime) / (endTime - startTime)
      OdeState[Y, D](t, startState.y.scale(1.0 - w).add(endState.y.scale(w)))
    }
  }
}

class EulerMethod[O <: Ode[Y, D], Y <: OdeYState[Y, D],
    D <: OdeDyDtState[D, Y]](ode: O)
    extends OdeStepper[LinearStep[Y, D], O, Y, D] {
  def step(state: OdeState[Y, D], t: Double) = {
    val dt = t - state.t
    val endState = OdeState[Y, D](t, state.y.add(ode.rhs(state).scale(dt)))
    LinearStep(state, endState)
  }
  def step(prevStep: LinearStep[Y, D], t: Double) = step(prevStep.endState, t)
}
