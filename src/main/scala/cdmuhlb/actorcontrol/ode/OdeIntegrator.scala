package cdmuhlb.actorcontrol.ode

trait OdeSolution[S <: OdeStep[Y, D], Y <: OdeYState[Y, D],
    D <: OdeDyDtState[D, Y]] {
  def startTime: Double
  def endTime: Double
  def containsTime(t: Double): Boolean
  def interpolate(t: Double): OdeState[Y, D]
  def lastStep: S
  def nSteps: Int
  def steps: Seq[S]
  def append(sol: OdeSolution[S, Y, D]): OdeSolution[S, Y, D]
}

class OdeSolutionBuilder[S <: OdeStep[Y, D], Y <: OdeYState[Y, D],
    D <: OdeDyDtState[D, Y]] {
  private var steps = Vector.empty[S]
  def appendStep(step: S): Unit = {
    assert(step.startTime < step.endTime)
    if (steps.nonEmpty) require(step.startTime == steps.last.endTime)
    steps :+= step
  }
  def toSolution: OdeSolution[S, Y, D] = new OdeSolutionImpl(steps)

  private class OdeSolutionImpl(val steps: Vector[S])
      extends OdeSolution[S, Y, D] {
    def startTime = steps.head.startTime
    def endTime = steps.last.endTime
    def containsTime(t: Double) = (t >= startTime) && (t <= endTime)
    def interpolate(t: Double) = {
      val stepOpt = search(t)
      require(stepOpt.nonEmpty)
      stepOpt.get.interpolate(t)
    }
    def lastStep = steps.last
    def nSteps = steps.length
    def append(sol: OdeSolution[S, Y, D]) = {
      require(sol.startTime == endTime)
      new OdeSolutionImpl(steps ++ sol.steps)
    }

    private def search(t: Double): Option[S] = {
      def binarySearch(lo: Int, hi: Int): Option[S] = {
        assert(lo <= hi)
        val i = (hi + lo)/2
        val step = steps(i)
        if (step.containsTime(t)) Some(step)
        else if (lo == hi) None
        else if (t < step.startTime) binarySearch(lo, i - 1)
        else binarySearch(i + 1, hi)
      }
      if ((t >= steps.head.startTime) && (t <= steps.last.endTime)) {
        binarySearch(0, steps.length-1)
      } else None
    }
  }
}

trait OdeIntegrator[S <: OdeStep[Y, D], O <: Ode[Y, D], Y <: OdeYState[Y, D],
    D <: OdeDyDtState[D, Y]] {
  def integrate(state: OdeState[Y, D], t: Double): OdeSolution[S, Y, D]
  def integrate(lastStep: S, t: Double): OdeSolution[S, Y, D]
}


class ConstantDtIntegrator[S <: OdeStep[Y, D], O <: Ode[Y, D],
    Y <: OdeYState[Y, D], D <: OdeDyDtState[D, Y]](
    stepper: OdeStepper[S, O, Y, D], maxDt: Double)
    extends OdeIntegrator[S, O, Y, D] {
  def integrate(state: OdeState[Y, D], t: Double) = {
    require(t > state.t)
    val duration = t - state.t
    val nSteps = math.ceil(duration/maxDt).toInt
    val dt = duration / nSteps
    val builder = new OdeSolutionBuilder[S, Y, D]
    if (nSteps == 1) builder.appendStep(stepper.step(state, t))
    else {
      val t1 = state.t + dt
      var step = stepper.step(state, t1)
      builder.appendStep(step)
      for (i ← 2 until nSteps) {
        val ti = state.t + i*dt
        step = stepper.step(step, ti)
        builder.appendStep(step)
      }
      builder.appendStep(stepper.step(step, t))
    }
    builder.toSolution
  }

  def integrate(lastStep: S, t: Double): OdeSolution[S, Y, D] = {
    require(t > lastStep.endTime)
    val duration = t - lastStep.endTime
    val nSteps = math.ceil(duration/maxDt).toInt
    val dt = duration / nSteps
    val builder = new OdeSolutionBuilder[S, Y, D]
    var step = lastStep
    for (i ← 1 until nSteps) {
      val ti = lastStep.endTime + i*dt
      step = stepper.step(step, ti)
      builder.appendStep(step)
    }
    builder.appendStep(stepper.step(step, t))
    builder.toSolution
  }
}

trait TwoStateErrorMeasurer[Y <: OdeYState[Y, D], D <: OdeDyDtState[D, Y]] {
  def measureError(y1: OdeState[Y, D], y2: OdeState[Y, D]): Double
}

class ScalarAbsRelErrorMeasurer(tolRatio: Double)
    extends TwoStateErrorMeasurer[ScalarYState, ScalarDyDtState] {
  require(tolRatio > 0.0)

  def measureError(y1: OdeState[ScalarYState, ScalarDyDtState],
      y2: OdeState[ScalarYState, ScalarDyDtState]) = {
    val delta = math.abs(y2.y.y - y1.y.y)
    val yabs = math.abs(y1.y.y).max(math.abs(y2.y.y))
    delta / (1.0 + tolRatio*yabs)
  }
}

class EmbeddedAdaptiveDtIntegrator[S <: OdeStep[Y, D], O <: Ode[Y, D],
    Y <: OdeYState[Y, D], D <: OdeDyDtState[D, Y]](
    stepper: EmbeddedOdeStepper[S, O, Y, D], minDt: Double,
    measurer: TwoStateErrorMeasurer[Y, D], tol: Double)
    extends OdeIntegrator[S, O, Y, D] {
  // From Numerical Recipes, Third Edition, p. 915
  private val beta = 0.4 / stepper.errorOrder
  private val alpha = 1.0 / stepper.errorOrder - 0.75*beta
  private val safetyFactor = 0.9
  private val minScale = 0.2
  private val maxScale = 10.0
  private val minLastErr = 1.0e-4

  def integrate(state: OdeState[Y, D], t: Double) = {
    val initialDt = 100.0*minDt // arbitrary
    val duration = t - state.t
    val builder = new OdeSolutionBuilder[S, Y, D]

    var dt = initialDt
    var lastErr = minLastErr

    // Take first step to initialize `lastStep`
    var ti = if ((state.t + dt) < t) (state.t + dt) else t
    dt = ti - state.t
    var (step, yy) = stepper.embeddedStep(state, ti)
    var err = measurer.measureError(step.endState, yy) / tol
    var rejected = err > 1.0
    while ((err > 1.0) && (dt > minDt)) {
      dt = nextDt(dt, err, lastErr).min(dt*safetyFactor)
      ti = state.t + dt
      val sy = stepper.embeddedStep(state, ti)
      step = sy._1
      yy = sy._2
      err = measurer.measureError(step.endState, yy) / tol
    }
    if (err > 1.0) { emitMinDtWarning(t, err) }
    builder.appendStep(step)
    var lastStep = step
    var lastT = ti
    dt = {
      val proposedDt = nextDt(dt, err, lastErr)
      if (rejected) proposedDt.min(dt)
      else proposedDt
    }
    lastErr = err.max(minLastErr)

    // Take remaining steps
    while (lastT < t) {
      ti = if ((lastT + dt) < t) (lastT + dt) else t
      dt = ti - lastT
      var sy = stepper.embeddedStep(lastStep, ti)
      step = sy._1
      yy = sy._2
      err = measurer.measureError(step.endState, yy) / tol
      rejected = err > 1.0
      while ((err > 1.0) && (dt > minDt)) {
        dt = nextDt(dt, err, lastErr).min(dt*safetyFactor)
        ti = lastT + dt
        sy = stepper.embeddedStep(lastStep, ti)
        step = sy._1
        yy = sy._2
        err = measurer.measureError(step.endState, yy) / tol
      }
      if (err > 1.0) { emitMinDtWarning(t, err) }
      builder.appendStep(step)
      lastStep = step
      lastT = ti
      dt = {
        val proposedDt = nextDt(dt, err, lastErr)
        if (rejected) proposedDt.min(dt)
        else proposedDt
      }
      lastErr = err.max(minLastErr)
    }

    builder.toSolution
  }

  def integrate(seedStep: S, t: Double) = {
    val initialDt = safetyFactor*seedStep.stepSize
    val builder = new OdeSolutionBuilder[S, Y, D]

    var lastT = seedStep.endTime
    var dt = initialDt
    var lastErr = minLastErr
    var lastStep = seedStep
    while (lastT < t) {
      var ti = if ((lastT + dt) < t) (lastT + dt) else t
      dt = ti - lastT
      var (step, yy) = stepper.embeddedStep(lastStep, ti)
      var err = measurer.measureError(step.endState, yy) / tol
      val rejected = err > 1.0
      while ((err > 1.0) && (dt > minDt)) {
        dt = nextDt(dt, err, lastErr).min(dt*safetyFactor)
        ti = lastT + dt
        val sy = stepper.embeddedStep(lastStep, ti)
        step = sy._1
        yy = sy._2
        err = measurer.measureError(step.endState, yy) / tol
      }
      if (err > 1.0) { emitMinDtWarning(t, err) }
      builder.appendStep(step)
      lastStep = step
      lastT = ti
      dt = {
        val proposedDt = nextDt(dt, err, lastErr)
        if (rejected) proposedDt.min(dt)
        else proposedDt
      }
      lastErr = err.max(minLastErr)
    }
    builder.toSolution
  }

  private def nextDt(dt: Double, err: Double, lastErr: Double): Double = {
    val scaledDt = {
      if (err == 0.0) dt*maxScale
      else {
        val piScale = safetyFactor*math.pow(err, -alpha)*math.pow(lastErr, beta)
        dt*piScale.max(minScale).min(maxScale)
      }
    }
    scaledDt.max(minDt)
  }

  private def emitMinDtWarning(t: Double, err: Double): Unit = {
    // TODO: use logging framework
    Console.err.println(s"[warn] ${getClass.getName}: Could not meet error tolerance without exceeding minDt")
    Console.err.println(s"       t: $t, err: ${err*tol}, tol: $tol, minDt: $minDt")
  }
}
