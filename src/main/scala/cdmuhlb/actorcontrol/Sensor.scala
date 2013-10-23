package cdmuhlb.actorcontrol

import scala.collection.immutable
import scala.concurrent.duration._
import akka.actor.{Actor, ActorLogging, ActorRef}

trait SensorModel {
  def measure(state: SystemState): Sensor.SensorReading
}

case object PerfectModel extends SensorModel {
  def measure(state: SystemState) = Sensor.SensorReading(state.t, state.theta,
      0.0)
}

case class GaussianModel(sig: Double) extends SensorModel {
  def measure(state: SystemState) = Sensor.SensorReading(state.t, state.theta +
      sig*util.Random.nextGaussian(), 0.0)
}

object Sensor {
  case object Sense
  case class SensorReading(t: Long, theta: Double, sigTheta: Double)
  case class Subscribe(subscriber: ActorRef)
  case class Unsubscribe(subscriber: ActorRef)
}

class Sensor(model: SensorModel, world: PhysicalSystem) extends Actor
    with ActorLogging {
  import Sensor._

  val senseSchedule = {
    implicit val ec = context.dispatcher
    context.system.scheduler.schedule(0.milliseconds, 100.milliseconds, self,
        Sense)
  }

  private var subscribers = immutable.ListSet.empty[ActorRef]

  def receive = {
    case Sense ⇒
      val timestamp = System.nanoTime
      val state = world.stateAt(timestamp)
      val reading = model.measure(state)
      for (a ← subscribers) a ! reading
    case Subscribe(a) ⇒ subscribers += a
    case Unsubscribe(a) ⇒ subscribers -= a
  }

  override def postStop = {
    senseSchedule.cancel()
  }
}
