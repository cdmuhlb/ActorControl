package cdmuhlb.actorcontrol

import akka.actor.{Props, ActorSystem}
import akka.testkit.{TestKit, TestActorRef, ImplicitSender}
import org.scalatest.{WordSpecLike, Matchers, BeforeAndAfterAll}

class SensorSpec extends TestKit(ActorSystem("SensorSpec")) with ImplicitSender
    with WordSpecLike with Matchers with BeforeAndAfterAll {
  override def afterAll = TestKit.shutdownActorSystem(system)

  "A Sensor" should {
    "send readings to subscribers" in {
      val testTheta = 1.2
      val sensor = system.actorOf(Props(classOf[Sensor], PerfectModel,
          new StationaryWorld(testTheta)))
      sensor ! Sensor.Subscribe(testActor)
      expectMsg(Sensor.SensorReading(testTheta, 0.0))
    }
  }
}
