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


case class ScalarDyDtState(dydt: Double)
    extends OdeDyDtState[ScalarDyDtState, ScalarYState] {
  def scale(dt: Double) = ScalarYState(dydt*dt)
}

case class ScalarYState(y: Double)
    extends OdeYState[ScalarYState, ScalarDyDtState] {
  def add(yState: ScalarYState) = ScalarYState(y + yState.y)
  def scale(scale: Double) = ScalarYState(y*scale)
  override def subtract(yState: ScalarYState) = ScalarYState(y - yState.y)
}

case class VectorDyDtState(dydt: Vector[Double])
    extends OdeDyDtState[VectorDyDtState, VectorYState] {
  def scale(dt: Double) = VectorYState(dydt map { _*dt })
}

case class VectorYState(y: Vector[Double])
    extends OdeYState[VectorYState, VectorDyDtState] {
  def add(yState: VectorYState) = VectorYState(
      for ((y1, y2) ← y.zip(yState.y)) yield y1 + y2)
  def scale(scale: Double) = VectorYState(y map { _*scale })
  override def subtract(yState: VectorYState) = VectorYState(
      for ((y1, y2) ← y.zip(yState.y)) yield y1 - y2)
}
