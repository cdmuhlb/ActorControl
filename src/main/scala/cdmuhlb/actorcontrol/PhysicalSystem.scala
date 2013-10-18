package cdmuhlb.actorcontrol

case class SystemState(t: Double, theta: Double, thetaDot: Double)

trait PhysicalSystem {
  def stateAt(t: Long): SystemState
}

class StationaryWorld(theta: Double) extends PhysicalSystem {
  def stateAt(t: Long) = SystemState(t, theta, 0.0)
}
