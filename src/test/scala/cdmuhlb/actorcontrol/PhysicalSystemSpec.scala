package cdmuhlb.actorcontrol

import org.scalatest.{WordSpec, Matchers}

class PhysicalSystemSpec extends WordSpec with Matchers {
  "A StationaryWorld" should {
    "always return the same position" in {
      val theta = 1.25
      val world = new StationaryWorld(theta)
      val t1 = 1000000000L
      val state1 = world.stateAt(t1)
      state1.t should equal (t1)
      state1.theta should equal (theta)
      world.updateTorque(1250000000L, 3.0)
      val t2 = 2000000000L
      val state2 = world.stateAt(t2)
      state2.t should equal (t2)
      state2.theta should equal (theta)
    }
    "return states with zero velocity" in {
      val theta = 1.25
      val world = new StationaryWorld(theta)
      val state1 = world.stateAt(1000000000L)
      state1.thetaDot should equal (0.0)
      world.updateTorque(1250000000L, 3.0)
      val state2 = world.stateAt(2000000000L)
      state2.thetaDot should equal (0.0)
    }
  }
  "A DeterministicWorld" should {
    "maintain equilibrium positions" in {
      val world1 = new DeterministicWorld(2.0, 0.5, SystemState(0L, 0.0, 0.0))
      val t1 = 3000000000L
      val state1 = world1.stateAt(t1)
      state1.t should equal (t1)
      state1.theta should be (0.0 +- 1.0e-15)
      state1.thetaDot should be (0.0 +- 1.0e-15)

      val g = 9.8
      val mass2 = 2.0
      val length2 = 0.5
      val theta2 = 0.5*math.Pi
      val tau2 = mass2*g*0.5*length2
      val world2 = new DeterministicWorld(mass2, length2,
          SystemState(0L, theta2, 0.0))
      world2.updateTorque(0L, tau2)
      val t2 = 3000000000L
      val state2 = world2.stateAt(t2)
      state2.t should equal (t2)
      state2.theta should be (theta2 +- 1.0e-15)
      state2.thetaDot should be (0.0 +- 1.0e-15)
    }
  }
}
