name := "ActorControl"

version := "1.0"

scalaVersion := "2.10.4"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.3.2",
  "com.typesafe.akka" %% "akka-testkit" % "2.3.2",
  "org.scalatest" %% "scalatest" % "2.1.3" % "test"
)

scalacOptions ++= Seq(
  "-target:jvm-1.7",
  "-deprecation",
  "-feature",
  "-unchecked"
)
