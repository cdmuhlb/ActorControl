package cdmuhlb.actorcontrol.ode

trait OdeDyDtState[D <: OdeDyDtState[D, Y], Y <: OdeYState[Y, D]] { this: D ⇒
  def scale(dt: Double): Y
}

trait OdeYState[Y <: OdeYState[Y, D], D <: OdeDyDtState[D, Y]] { this: Y ⇒
  def add(y: Y): Y
  def scale(scale: Double): Y
  def subtract(y: Y): Y = add(y.scale(-1.0))
}

case class OdeState[Y <: OdeYState[Y, D], D <: OdeDyDtState[D, Y]](
    t: Double, y: Y)

trait Ode[Y <: OdeYState[Y, D], D <: OdeDyDtState[D, Y]] {
  def rhs(state: OdeState[Y, D]): D
}
