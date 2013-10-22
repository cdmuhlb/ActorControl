package cdmuhlb.actorcontrol.physics

import org.scalatest.{WordSpec, Matchers}
import cdmuhlb.actorcontrol.ode._

class PendulumSpec extends WordSpec with Matchers {
  "A RodPendulumOde" should {
    "compute correct positions for horizontal alignments" in {
      val length = 2.5
      val ode = new RodPendulumOde(1.0, length, 0.0)
      // Down
      val state1 = OdeState[PendulumYState, PendulumDyDtState](0.0,
          PendulumYState(0.5*math.Pi, 3.0))
      val pos1 = ode.tipPos(state1)
      pos1._1 should be (length +- 1.0e-15)
      pos1._2 should be (0.0 +- 1.0e-15)

      // Up
      val state2 = OdeState[PendulumYState, PendulumDyDtState](2.0,
          PendulumYState(-0.5*math.Pi, -3.0))
      val pos2 = ode.tipPos(state2)
      pos2._1 should be (-length +- 1.0e-15)
      pos2._2 should be (0.0 +- 1.0e-15)
    }
    "compute correct positions for vertical alignments" in {
      val length = 2.5
      val ode = new RodPendulumOde(1.0, length, 0.0)
      // Down
      val state1 = OdeState[PendulumYState, PendulumDyDtState](0.0,
          PendulumYState(0.0, 3.0))
      val pos1 = ode.tipPos(state1)
      pos1._1 should be (0.0 +- 1.0e-15)
      pos1._2 should be (-length +- 1.0e-15)

      // Up
      val state2 = OdeState[PendulumYState, PendulumDyDtState](2.0,
          PendulumYState(math.Pi, -3.0))
      val pos2 = ode.tipPos(state2)
      pos2._1 should be (0.0 +- 1.0e-15)
      pos2._2 should be (length +- 1.0e-15)
    }
    "have no kinetic energy when at rest" in {
      val state = OdeState[PendulumYState, PendulumDyDtState](0.0,
          PendulumYState(0.5, 0.0))
      val ode1 = new RodPendulumOde(1.0, 1.0, 0.0)
      ode1.kineticEnergy(state) should equal (0.0)
      val ode2 = new RodPendulumOde(2.0, 3.0, 5.0)
      ode2.kineticEnergy(state) should equal (0.0)
    }
    "have positive kinetic energy when moving" in {
      val ode = new RodPendulumOde(1.0, 1.0, 0.0)
      val state1 = OdeState[PendulumYState, PendulumDyDtState](0.0,
          PendulumYState(-0.5, 1.0))
      ode.kineticEnergy(state1) should be > 0.0
      val state2 = OdeState[PendulumYState, PendulumDyDtState](0.0,
          PendulumYState(0.5, -7.0))
      ode.kineticEnergy(state2) should be > 0.0
    }
    "conserve energy when evolved with no external torque" in {
      val tau = 0.0
      val ode = new RodPendulumOde(1.0, 1.0, tau)
      val t0 = 0.0
      val state0 = OdeState[PendulumYState, PendulumDyDtState](t0,
          PendulumYState(0.5, 0.0))
      val e0 = ode.totalEnergy(state0)

      val tol = 1.0e-8
      val minDt = 1.0e-6
      val measurer = new PendulumAbsRelErrorMeasurer(1.0)
      val stepper = new BogackiShampineMethod[RodPendulumOde, PendulumYState,
          PendulumDyDtState](ode)
      val solver = new EmbeddedAdaptiveDtIntegrator(stepper, minDt, measurer,
          tol)

      val t1 = 10.0
      val sol = solver.integrate(state0, t1)
      val state1 = sol.interpolate(t1)
      val e1 = ode.totalEnergy(state1)
      e1 should be (e0 +- 1.0e-6)

      val statei = sol.interpolate(0.5*(t0 + t1))
      val ei = ode.totalEnergy(statei)
      ei should be (e0 +- 1.0e-6)
    }
  }
}
