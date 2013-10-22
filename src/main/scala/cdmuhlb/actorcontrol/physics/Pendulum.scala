package cdmuhlb.actorcontrol.physics

import cdmuhlb.actorcontrol.ode.{OdeDyDtState, OdeYState, OdeState, Ode,
    TwoStateErrorMeasurer}

case class PendulumDyDtState(thetaDot: Double, omegaDot: Double)
    extends OdeDyDtState[PendulumDyDtState, PendulumYState] {
  def scale(dt: Double) = PendulumYState(thetaDot*dt, omegaDot*dt)
}

case class PendulumYState(theta: Double, omega: Double)
    extends OdeYState[PendulumYState, PendulumDyDtState] {
  // `theta` represents total accumulated phase (or change thereof)
  def add(y: PendulumYState) = PendulumYState(theta + y.theta, omega + y.omega)
  def scale(scale: Double) = PendulumYState(theta*scale, omega*scale)
  override def subtract(y: PendulumYState) =
      PendulumYState(theta - y.theta, omega - y.omega)
}

case class PendulumAbsRelErrorMeasurer(tolRatio: Double)
    extends TwoStateErrorMeasurer[PendulumYState, PendulumDyDtState] {
  require(tolRatio > 0.0)

  def measureError(y1: OdeState[PendulumYState, PendulumDyDtState],
      y2: OdeState[PendulumYState, PendulumDyDtState]) = {
    val errTheta = {
      val delta = math.abs(y2.y.theta - y1.y.theta)
      val yabs = math.abs(y1.y.theta).max(math.abs(y2.y.theta))
      delta / (1.0 + tolRatio*yabs)
    }
    val errOmega = {
      val delta = math.abs(y2.y.omega - y1.y.omega)
      val yabs = math.abs(y1.y.omega).max(math.abs(y2.y.omega))
      delta / (1.0 + tolRatio*yabs)
    }
    math.sqrt(0.5*(errTheta*errTheta + errOmega*errOmega))
  }
}

class RodPendulumOde(mass: Double, length: Double, tau: Double)
    extends Ode[PendulumYState, PendulumDyDtState] {
  val g = 9.8;
  def rhs(state: OdeState[PendulumYState, PendulumDyDtState]) =
      PendulumDyDtState(state.y.omega, 3.0/(mass*length*length) *
      (tau - 0.5*g*mass*length*math.sin(state.y.theta)))

  def potentialEnergy(state: OdeState[PendulumYState, PendulumDyDtState]):
      Double = 0.5*g*mass*length*(1.0 - math.cos(state.y.theta))
  def kineticEnergy(state: OdeState[PendulumYState, PendulumDyDtState]):
      Double = mass*length*length*state.y.omega*state.y.omega/6.0
  def totalEnergy(state: OdeState[PendulumYState, PendulumDyDtState]): Double =
      potentialEnergy(state) + kineticEnergy(state)
}
