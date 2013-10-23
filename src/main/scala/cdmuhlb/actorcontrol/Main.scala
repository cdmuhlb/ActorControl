package cdmuhlb.actorcontrol

import akka.actor.{ActorSystem, Props}
import akka.actor.{Actor, ActorLogging, ActorRef}

object Main extends App {
  val system = ActorSystem("ActorControl")
  val mass = 1.0
  val length = 1.0
  val theta0 = 1.0
  val world = new DeterministicWorld(mass, length, 
      SystemState(System.nanoTime, theta0, 0.0))
  val sensor = system.actorOf(Props(classOf[Sensor], PerfectModel, world))
  val echo = system.actorOf(Props[SensorEcho])
  sensor ! Sensor.Subscribe(echo)

  // Wait for user input
  readLine()
  sensor ! Sensor.Unsubscribe(echo)
  system.shutdown()
}

class SensorEcho extends Actor {
  def receive = {
    case Sensor.SensorReading(t, theta, sigTheta) ⇒
      println(s"$t  $theta")
  }
}
