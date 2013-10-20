package cdmuhlb.actorcontrol.ode

case class BogackiShampineStep[Y <: OdeYState[Y, D],
    D <: OdeDyDtState[D, Y]](startState: OdeState[Y, D], k1: Y,
    endState: OdeState[Y, D], k4: Y) extends OdeStep[Y, D] {
  val startTime = startState.t
  val endTime = endState.t
  def interpolate(t: Double) = {
    require(containsTime(t))
    if (t == startTime) startState
    else if (t == endTime) endState
    else {
      val w = (t - startTime) / (endTime - startTime)
      OdeState[Y, D](t, startState.y.scale(1.0 - w).add(endState.y.scale(
          w)).add((endState.y.subtract(startState.y).scale(1.0 - 2.0*w).add(
          k1.scale(w - 1.0)).add(k4.scale(w))).scale(w*(w - 1.0))))
    }
  }
}

class BogackiShampineMethod[O <: Ode[Y, D], Y <: OdeYState[Y, D],
    D <: OdeDyDtState[D, Y]](ode: O)
    extends EmbeddedOdeStepper[BogackiShampineStep[Y, D], O, Y, D] {
  def step(state: OdeState[Y, D], t: Double, measurer: ErrorMeasurer[Y, D]) = {
    val h = t - state.t
    val k1 = ode.rhs(state).scale(h)
    val (nextSeg, z1) = fsalStep(state, k1, t)
    (nextSeg, measurer.measureError(nextSeg.endState, z1))
  }

  def step(lastStep: BogackiShampineStep[Y, D], t: Double,
      measurer: ErrorMeasurer[Y, D]) = {
    val (nextSeg, z1) = fsalStep(lastStep.endState, lastStep.k4, t)
    (nextSeg, measurer.measureError(nextSeg.endState, z1))
  }

  def step(state: OdeState[Y, D], t: Double) = {
    val h = t - state.t
    val k1 = ode.rhs(state).scale(h)
    val (nextSeg, z1) = fsalStep(state, k1, t)
    nextSeg
  }

  def step(lastStep: BogackiShampineStep[Y, D], t: Double) = {
    val (nextSeg, z1) = fsalStep(lastStep.endState, lastStep.k4, t)
    nextSeg
  }

  private def fsalStep(state: OdeState[Y, D], k1: Y, t: Double):
      (BogackiShampineStep[Y, D], OdeState[Y, D]) = {
    val h = t - state.t
    val k2 = ode.rhs(OdeState(state.t + 0.5*h,
        state.y.add(k1.scale(0.5)))).scale(h)
    val k3 = ode.rhs(OdeState(state.t + 0.75*h,
        state.y.add(k2.scale(0.75)))).scale(h)
    val endState = OdeState[Y, D](t, state.y.add(k1.scale(2.0/9.0)).add(
        k2.scale(1.0/3.0)).add(k3.scale(4.0/9.0)))
    val k4 = ode.rhs(endState).scale(h)
    val thisStep = BogackiShampineStep(state, k1, endState, k4)
    val z1 = OdeState[Y, D](t, state.y.add(k1.scale(7.0/24.0)).add(
        k2.scale(0.25)).add(k3.scale(1.0/3.0)).add(k4.scale(1.0/8.0)))
    (thisStep, z1)
  }
}
