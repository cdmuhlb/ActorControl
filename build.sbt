name := "ActorControl"

version := "1.0"

scalaVersion := "2.10.3"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.2.1",
  "com.typesafe.akka" %% "akka-testkit" % "2.2.1",
  "org.scalatest" %% "scalatest" % "2.0.RC2" % "test"
)

scalacOptions ++= Seq(
  "-target:jvm-1.7",
  "-deprecation",
  "-feature",
  "-unchecked"
)
