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
}
