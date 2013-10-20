package cdmuhlb.actorcontrol.ode

class LinearRk4Method[O <: Ode[Y, D], Y <: OdeYState[Y, D],
    D <: OdeDyDtState[D, Y]](ode: O)
    extends OdeStepper[LinearStep[Y, D], O, Y, D] {
  def step(state: OdeState[Y, D], t: Double) = {
    val h = t - state.t
    val k1 = ode.rhs(state).scale(h)
    val k2 = ode.rhs(OdeState(state.t + 0.5*h,
        state.y.add(k1.scale(0.5)))).scale(h)
    val k3 = ode.rhs(OdeState(state.t + 0.5*h,
        state.y.add(k2.scale(0.5)))).scale(h)
    val k4 = ode.rhs(OdeState(t, state.y.add(k3))).scale(h)
    val endState = OdeState[Y, D](t, state.y.add(k1.scale(1.0/6.0)).add(
        k2.scale(1.0/3.0)).add(k3.scale(1.0/3.0)).add(k4.scale(1.0/6.0)))
    LinearStep(state, endState)
  }
  def step(prevStep: LinearStep[Y, D], t: Double) = step(prevStep.endState, t)
}
