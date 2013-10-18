package cdmuhlb.actorcontrol

case class SystemState(t: Double, theta: Double, thetaDot: Double)

trait PhysicalSystem {
  def stateAt(t: Long): SystemState
  def updateForce(t: Long, force: Double): Unit
}

class StationaryWorld(theta: Double) extends PhysicalSystem {
  def stateAt(t: Long) = SystemState(t, theta, 0.0)
  def updateForce(t: Long, force: Double) = { }
}
