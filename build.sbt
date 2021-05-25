import sbt.Keys.libraryDependencies

name := "binarytree"

version := "0.1"

scalaVersion := "2.13.6"

idePackagePrefix := Some("com.binarytree")

val AkkaVersion = "2.6.14"

lazy val root = (project in file("."))
  .settings(
    name := "Binary tree"
  )
lazy val typed = (project in file("typed"))
  .settings(
    name := "Typed binary tree",
    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-actor-typed" % AkkaVersion,
      "com.typesafe.akka" %% "akka-slf4j" % AkkaVersion,
      "ch.qos.logback" % "logback-classic" % "1.2.3",
      "com.typesafe.akka" %% "akka-actor-testkit-typed" % AkkaVersion % Test
    )
  )
lazy val classic = (project in file("classic"))
  .settings(
    name := "Classic binary tree",
    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-actor" % AkkaVersion,
      "com.typesafe.akka" %% "akka-slf4j" % AkkaVersion,
      "ch.qos.logback" % "logback-classic" % "1.2.3",
      "com.typesafe.akka" %% "akka-testkit" % AkkaVersion % Test
    )
  )