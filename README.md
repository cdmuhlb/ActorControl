ActorControl
============
A project to implement a control system using actors.  The controller will
asynchronously process measurements from sensors and submit instructions to
actuators while updating its own physical model and uncertainties.

Of particular note is the `cdmuhlb.actorcontrol.ode` package, which implements
a modern and abstract interface for solving ordinary differential equations with
explicit timesteppers.  It is inspired by chapter 17 of _Numerical Recipes,
Third Edition_ and features adaptive timestepping and dense output.

Dependencies
------------
_ActorControl_ is written in Scala.  Combilation requires an
[SBT](http://www.scala-sbt.org/) launcher compatible with version 0.13.2 and a
Java SE 7 JDK.  Execution only requires a Java SE 7 JVM.
