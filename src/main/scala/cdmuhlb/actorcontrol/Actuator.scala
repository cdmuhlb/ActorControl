package cdmuhlb.actorcontrol

import akka.actor.{Actor, ActorLogging}

object Actuator {
  case class ActuatorSetting(force: Double)
}

class Actuator(world: PhysicalSystem) extends Actor with ActorLogging {
  import Actuator._

  def receive = {
    case ActuatorSetting(force) ⇒
      val timestamp = System.nanoTime
      world.updateForce(timestamp, force)
  }
}
