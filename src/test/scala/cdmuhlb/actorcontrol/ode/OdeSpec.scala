package cdmuhlb.actorcontrol.ode

import org.scalatest.{WordSpec, Matchers}

class OdeSpec extends WordSpec with Matchers {
  "A ScalarDyDtState" should {
    "scale to the correct ScalarYState" in {
      val dydt = 2.0
      val dt = 5.0
      val y = dydt*dt

      val dydtState = ScalarDyDtState(dydt)
      val yState = dydtState.scale(dt)
      yState should equal (ScalarYState(y))
    }
  }

  "A ScalarYState" should {
    "scale to the correct ScalarYState" in {
      val y0 = 1.5
      val scale = 3.0
      val y1 = y0*scale

      val y0State = ScalarYState(y0)
      val y1State = y0State.scale(scale)
      y1State should equal (ScalarYState(y1))
    }
    "add correctly to another ScalarYState" in {
      val y0 = -0.5
      val y1 = 4.25
      val y2 = y0 + y1

      val y0State = ScalarYState(y0)
      val y1State = ScalarYState(y1)
      val y2State = y0State.add(y1State)
      y2State should equal (ScalarYState(y2))
    }
    "subtract correctly from another ScalarYState" in {
      val y0 = -9.25
      val y1 = 6.75
      val y2 = y0 - y1

      val y0State = ScalarYState(y0)
      val y1State = ScalarYState(y1)
      val y2State = y0State.subtract(y1State)
      y2State should equal (ScalarYState(y2))
    }
  }

  "A VectorDyDtState" should {
    "scale to the correct VectorYState" in {
      val dydtV1 = 4.5
      val dydtV2 = -3.5
      val dt = 4.0
      val yV1 = dydtV1*dt
      val yV2 = dydtV2*dt

      val dydtState = VectorDyDtState(Vector(dydtV1, dydtV2))
      val yState = dydtState.scale(dt)
      yState should equal (VectorYState(Vector(yV1, yV2)))
    }
  }
  "A VectorYState" should {
    "scale to the correct VectorYState" in {
      val y0V1 = 1.0
      val y0V2 = -2.5
      val scale = 3.75
      val y1V1 = y0V1*scale
      val y1V2 = y0V2*scale

      val y0State = VectorYState(Vector(y0V1, y0V2))
      val y1State = y0State.scale(scale)
      y1State should equal (VectorYState(Vector(y1V1, y1V2)))
    }
    "add correctly to another VectorYState" in {
      val y0V1 = 1.0
      val y0V2 = 2.5
      val y1V1 = 3.75
      val y1V2 = 4.875
      val y2V1 = y0V1 + y1V1
      val y2V2 = y0V2 + y1V2

      val y0State = VectorYState(Vector(y0V1, y0V2))
      val y1State = VectorYState(Vector(y1V1, y1V2))
      val y2State = y0State.add(y1State)
      y2State should equal (VectorYState(Vector(y2V1, y2V2)))
    }
    "subtract correctly from another VectorYState" in {
      val y0V1 = 4.0
      val y0V2 = 3.5
      val y1V1 = 2.75
      val y1V2 = 1.875
      val y2V1 = y0V1 - y1V1
      val y2V2 = y0V2 - y1V2

      val y0State = VectorYState(Vector(y0V1, y0V2))
      val y1State = VectorYState(Vector(y1V1, y1V2))
      val y2State = y0State.subtract(y1State)
      y2State should equal (VectorYState(Vector(y2V1, y2V2)))
    }
  }
}
