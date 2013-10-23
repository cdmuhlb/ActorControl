package cdmuhlb.actorcontrol

import akka.actor.{Actor, ActorLogging}

object Actuator {
  case class ActuatorSetting(tau: Double)
}

class Actuator(world: PhysicalSystem) extends Actor with ActorLogging {
  import Actuator._

  def receive = {
    case ActuatorSetting(tau) ⇒
      val timestamp = System.nanoTime
      world.updateTorque(timestamp, tau)
  }
}
