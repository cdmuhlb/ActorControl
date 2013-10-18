package cdmuhlb.actorcontrol

case class SystemState(theta: Double, thetaDot: Double)

trait PhysicalSystem {
  def stateAt(t: Long): SystemState
}

class StationaryWorld(theta: Double) extends PhysicalSystem {
  def stateAt(t: Long) = SystemState(theta, 0.0)
}
