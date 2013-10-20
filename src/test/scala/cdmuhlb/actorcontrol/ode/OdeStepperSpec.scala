package cdmuhlb.actorcontrol.ode

import org.scalatest.{WordSpec, Matchers}

class ConstantOde(dydt: Double) extends Ode[ScalarYState, ScalarDyDtState] {
  def rhs(state: OdeState[ScalarYState, ScalarDyDtState]) =
      ScalarDyDtState(dydt)
}

class OdeStepperSpec extends WordSpec with Matchers {
  "A LinearStep" should {
    "interpolate perfectly to boundaries" in {
      val t0 = 0.0
      val t1 = 3.0
      val state0 = OdeState[ScalarYState, ScalarDyDtState](t0,
          ScalarYState(2.5))
      val state1 = OdeState[ScalarYState, ScalarDyDtState](t1,
          ScalarYState(5.0))
      val step = LinearStep(state0, state1)
      step.interpolate(t0) should equal (state0)
      step.interpolate(t1) should equal (state1)
    }
    "interpolate linearly in the interior" in {
      val t0 = 0.0
      val y0 = 0.0
      val t1 = 1.0
      val y1 = 2.0
      val state0 = OdeState[ScalarYState, ScalarDyDtState](t0, ScalarYState(y0))
      val state1 = OdeState[ScalarYState, ScalarDyDtState](t1, ScalarYState(y1))
      val step = LinearStep(state0, state1)
      val ti = 0.25
      val yi = 0.5
      val ans = step.interpolate(ti)
      ans.t should equal (ti)
      ans.y.y should be (yi +- 1.0e-15)
    }
  }

  "An EulerMethod" should {
    "compute linear solutions exactly" in {
      val dydt = 2.0
      val ode = new ConstantOde(dydt)
      val stepper = new EulerMethod[ConstantOde, ScalarYState,
          ScalarDyDtState](ode)
      val t0 = 1.0
      val y0 = 0.0
      val state0 = OdeState[ScalarYState, ScalarDyDtState](t0, ScalarYState(y0))
      val t1 = 3.0
      val y1 = y0 + dydt*(t1 - t0)
      val step = stepper.step(state0, t1)
      step.endTime should equal (t1)
      step.endState.y.y should be (y1 +- 1.0e-15)
    }
  }

  "A LinearRk4Method" should {
    "compute linear solutions exactly" in {
      val dydt = 2.0
      val ode = new ConstantOde(dydt)
      val stepper = new LinearRk4Method[ConstantOde, ScalarYState,
          ScalarDyDtState](ode)
      val t0 = 1.0
      val y0 = 0.0
      val state0 = OdeState[ScalarYState, ScalarDyDtState](t0, ScalarYState(y0))
      val t1 = 3.0
      val y1 = y0 + dydt*(t1 - t0)
      val step = stepper.step(state0, t1)
      step.endTime should equal (t1)
      step.endState.y.y should be (y1 +- 1.0e-15)
    }
  }

  "A BogackiShampineStep" should {
    "interpolate perfectly to boundaries" in {
      val dydt = 2.0
      val ode = new ConstantOde(dydt)
      val stepper = new BogackiShampineMethod[ConstantOde, ScalarYState,
          ScalarDyDtState](ode)
      val t0 = 1.0
      val y0 = 0.0
      val state0 = OdeState[ScalarYState, ScalarDyDtState](t0, ScalarYState(y0))
      val t1 = 3.0
      val y1 = y0 + dydt*(t1 - t0)
      val step = stepper.step(state0, t1)
      step.interpolate(t0) should equal (state0)
      step.interpolate(t1) should equal (step.endState)
    }
    "interpolate linear solutions exactly" in {
      val dydt = 2.0
      val ode = new ConstantOde(dydt)
      val stepper = new BogackiShampineMethod[ConstantOde, ScalarYState,
          ScalarDyDtState](ode)
      val t0 = 1.0
      val y0 = 0.0
      val state0 = OdeState[ScalarYState, ScalarDyDtState](t0, ScalarYState(y0))
      val t1 = 3.0
      val step = stepper.step(state0, t1)
      val ti = 1.7
      val yi = y0 + dydt*(ti - t0)
      step.interpolate(ti).y.y should be (yi +- 1.0e-15)
    }
  }
  "A BogackiShampineMethod" should {
    "compute linear solutions exactly" in {
      val dydt = 2.0
      val ode = new ConstantOde(dydt)
      val stepper = new BogackiShampineMethod[ConstantOde, ScalarYState,
          ScalarDyDtState](ode)
      val t0 = 1.0
      val y0 = 0.0
      val state0 = OdeState[ScalarYState, ScalarDyDtState](t0, ScalarYState(y0))
      val t1 = 3.0
      val y1 = y0 + dydt*(t1 - t0)
      val step = stepper.step(state0, t1)
      step.endTime should equal (t1)
      step.endState.y.y should be (y1 +- 1.0e-15)
    }
    "compute correct steps from an OdeStep" in {
      val dydt = 2.0
      val ode = new ConstantOde(dydt)
      val stepper = new BogackiShampineMethod[ConstantOde, ScalarYState,
          ScalarDyDtState](ode)
      val t0 = 1.0
      val y0 = 0.0
      val state0 = OdeState[ScalarYState, ScalarDyDtState](t0, ScalarYState(y0))
      val t1 = 2.0
      val step1 = stepper.step(state0, t1)
      val t2 = 5.0
      val y2 = y0 + dydt*(t2 - t0)
      val step2 = stepper.step(step1, t2)
      step2.endTime should equal (t2)
      step2.endState.y.y should be (y2 +- 1.0e-15)
    }
  }
}
