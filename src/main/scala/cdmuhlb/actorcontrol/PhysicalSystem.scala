package cdmuhlb.actorcontrol

import cdmuhlb.actorcontrol.ode._
import cdmuhlb.actorcontrol.physics._

case class SystemState(t: Long, theta: Double, thetaDot: Double)

trait PhysicalSystem {
  def stateAt(t: Long): SystemState
  def updateTorque(t: Long, tau: Double): Unit
}

class StationaryWorld(theta: Double) extends PhysicalSystem {
  def stateAt(t: Long) = SystemState(t, theta, 0.0)
  def updateTorque(t: Long, tau: Double) = { }
}

class DeterministicWorld(mass: Double, length: Double, state0: SystemState)
    extends PhysicalSystem {
  val tol = 1.0e-8
  val minDt = 1.0e-6
  val measurer = new PendulumAbsRelErrorMeasurer(1.0)

  var ode = new RodPendulumOde(mass, length, 0.0)
  var stepper = new BogackiShampineMethod[RodPendulumOde, PendulumYState,
        PendulumDyDtState](ode)
  var solver = new EmbeddedAdaptiveDtIntegrator(stepper, minDt, measurer, tol)

  private var solOpt: Option[OdeSolution[BogackiShampineStep[PendulumYState,
      PendulumDyDtState], PendulumYState, PendulumDyDtState]] = None

  def stateAt(tns: Long) = {
    require(tns - state0.t >= 0)
    val t = worldToOdeTime(tns)
    if (t == worldToOdeTime(state0.t)) state0
    else {
      if (t > currentT) integrateTo(t)
      odeToWorldState(solOpt.get.interpolate(t))
    }
  }

  def updateTorque(tns: Long, tau: Double) = {
    val t = worldToOdeTime(tns)
    require(t >= currentT)
    if (t > currentT) integrateTo(t)

    ode = new RodPendulumOde(mass, length, tau)
    stepper = new BogackiShampineMethod[RodPendulumOde, PendulumYState,
        PendulumDyDtState](ode)
    solver = new EmbeddedAdaptiveDtIntegrator(stepper, minDt, measurer, tol)
  }

  private def currentT: Double = solOpt.map(_.endTime).getOrElse(0.0)

  private def integrateTo(t: Double): Unit = {
    assert(t > currentT)
    solOpt match {
      case None ⇒
        val odeState = OdeState[PendulumYState, PendulumDyDtState](
            0.0, PendulumYState(state0.theta, state0.thetaDot))
        solOpt = Some(solver.integrate(odeState, t))
      case Some(sol) ⇒
        val sol = solOpt.get
        val nextSol = solver.integrate(sol.lastStep, t)
        solOpt = Some(sol.append(nextSol))
    }
  }

  private def worldToOdeTime(tns: Long): Double = 1.0e-9*(tns - state0.t)
  private def odeToWorldTime(t: Double): Long = math.round(state0.t + 1.0e9*t)

  private def odeToWorldState(
      odeState: OdeState[PendulumYState, PendulumDyDtState]): SystemState = {
    SystemState(odeToWorldTime(odeState.t), odeState.y.theta, odeState.y.omega)
  }
}
