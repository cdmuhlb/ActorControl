package cdmuhlb.actorcontrol.ode

import org.scalatest.{WordSpec, Matchers}

class OdeIntegratorSpec extends WordSpec with Matchers {
  "An OdeSolutionBuilder" should {
    "build OdeSolutions containing the correct times" in {
      val t0 = 0.0
      val t1 = 3.0
      val t2 = 4.5
      val t3 = 9.25
      val state0 = OdeState[ScalarYState, ScalarDyDtState](t0,
          ScalarYState(2.5))
      val state1 = OdeState[ScalarYState, ScalarDyDtState](t1,
          ScalarYState(5.0))
      val state2 = OdeState[ScalarYState, ScalarDyDtState](t2,
          ScalarYState(3.0))
      val state3 = OdeState[ScalarYState, ScalarDyDtState](t3,
          ScalarYState(8.5))
      val step1 = LinearStep(state0, state1)
      val step2 = LinearStep(state1, state2)
      val step3 = LinearStep(state2, state3)

      val builder = new OdeSolutionBuilder[
          LinearStep[ScalarYState, ScalarDyDtState],
          ScalarYState, ScalarDyDtState]
      builder.appendStep(step1)
      builder.appendStep(step2)
      builder.appendStep(step3)

      val sol = builder.toSolution
      sol.containsTime(t0) should be (true)
      sol.containsTime(t3) should be (true)
      sol.containsTime(0.5*(t1 + t2)) should be (true)
      sol.containsTime(t0 - 1.0) should be (false)
      sol.containsTime(t3 + 1.0) should be (false)
    }
  }

  "An OdeSolution" should {
    "correctly return the last step" in {
      val t0 = 1.0
      val t1 = 5.0
      val t2 = 7.5
      val state0 = OdeState[ScalarYState, ScalarDyDtState](t0,
          ScalarYState(3.5))
      val state1 = OdeState[ScalarYState, ScalarDyDtState](t1,
          ScalarYState(4.0))
      val state2 = OdeState[ScalarYState, ScalarDyDtState](t2,
          ScalarYState(-3.0))
      val step1 = LinearStep(state0, state1)
      val step2 = LinearStep(state1, state2)

      val builder = new OdeSolutionBuilder[
          LinearStep[ScalarYState, ScalarDyDtState],
          ScalarYState, ScalarDyDtState]
      builder.appendStep(step1)
      builder.appendStep(step2)

      val sol = builder.toSolution
      sol.lastStep should equal(step2)
    }
  }

  "A ConstantDtIntegrator" should {
    "compute linear solutions exactly with Euler's method" in {
      val dydt = 3.0
      val ode = new ConstantOde(dydt)
      val stepper = new EulerMethod[ConstantOde, ScalarYState,
          ScalarDyDtState](ode)
      val t0 = 0.0
      val y0 = 1.0
      val state0 = OdeState[ScalarYState, ScalarDyDtState](t0, ScalarYState(y0))
      val t1 = 5.0
      val y1 = y0 + dydt*(t1 - t0)

      val maxDt = 0.7
      val solver = new ConstantDtIntegrator(stepper, maxDt)
      val sol = solver.integrate(state0, t1)
      val solState = sol.interpolate(t1)
      solState.t should equal (t1)
      solState.y.y should be (y1 +- 1.0e-15)

      val minSteps = math.ceil((t1 - t0)/maxDt).toInt
      sol.nSteps should be >= (minSteps)
    }
    "compute correct solutions when starting from an OdeStep" in {
      val dydt = 3.0
      val ode = new ConstantOde(dydt)
      val stepper = new BogackiShampineMethod[ConstantOde, ScalarYState,
          ScalarDyDtState](ode)
      val t0 = 0.0
      val y0 = 1.0
      val state0 = OdeState[ScalarYState, ScalarDyDtState](t0, ScalarYState(y0))
      val t1 = 0.1
      val step1 = stepper.step(state0, t1)
      val t2 = 5.0

      val maxDt = 0.7
      val solver = new ConstantDtIntegrator(stepper, maxDt)
      val stepSol = solver.integrate(step1, t2)
      val stateSol = solver.integrate(step1.endState, t2)
      val stepSolState = stepSol.interpolate(t2)
      val stateSolState = stateSol.interpolate(t2)
      stepSolState.t should equal (t2)
      stepSolState.y.y should be (stateSolState.y.y +- 1.0e-15)
    }
  }

  "An EmbeddedAdaptiveDtIntegrator" should {
    "compute linear solutions exactly with the Bogacki-Shampine method" in {
      val dydt = 3.0
      val ode = new ConstantOde(dydt)
      val stepper = new BogackiShampineMethod[ConstantOde, ScalarYState,
          ScalarDyDtState](ode)
      val t0 = 0.0
      val y0 = 1.0
      val state0 = OdeState[ScalarYState, ScalarDyDtState](t0, ScalarYState(y0))
      val t1 = 5.0
      val y1 = y0 + dydt*(t1 - t0)

      val tol = 1.0e-6
      val minDt = 1.0e-8
      val measurer = new ScalarAbsRelErrorMeasurer(1.0)
      val solver = new EmbeddedAdaptiveDtIntegrator(stepper, minDt, measurer,
          tol)
      val sol = solver.integrate(state0, t1)
      val solState = sol.interpolate(t1)
      solState.t should equal (t1)
      solState.y.y should be (y1 +- 1.0e-15)
    }
    "compute correct solutions when starting from an OdeStep" in {
      val dydt = 3.0
      val ode = new ConstantOde(dydt)
      val stepper = new BogackiShampineMethod[ConstantOde, ScalarYState,
          ScalarDyDtState](ode)
      val t0 = 0.0
      val y0 = 1.0
      val state0 = OdeState[ScalarYState, ScalarDyDtState](t0, ScalarYState(y0))
      val t1 = 0.1
      val step1 = stepper.step(state0, t1)
      val t2 = 5.0

      val tol = 1.0e-6
      val minDt = 1.0e-8
      val measurer = new ScalarAbsRelErrorMeasurer(1.0)
      val solver = new EmbeddedAdaptiveDtIntegrator(stepper, minDt, measurer,
          tol)
      val stepSol = solver.integrate(step1, t2)
      val stateSol = solver.integrate(step1.endState, t2)
      val stepSolState = stepSol.interpolate(t2)
      val stateSolState = stateSol.interpolate(t2)
      stepSolState.t should equal (t2)
      stepSolState.y.y should be (stateSolState.y.y +- 1.0e-15)
    }
  }
}
