name := "Go Engine"

organization := "net.semeai"

version := "0.0.1"

scalaVersion := "2.10.3"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.0.3",
  "joda-time" % "joda-time" % "2.3",
  "org.joda" % "joda-convert" % "1.5" % "compile",
  "org.specs2" %% "specs2" % "2.2.3" % "test"
)

scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-feature",
  "-Ywarn-all",
  "-Ywarn-value-discard",
  "-Ywarn-numeric-widen",
  "-Ywarn-dead-code",
  "-Yinline-warnings",
  "-Xlint",
  "-Xfatal-warnings",
  "-optimise",
  "-language:implicitConversions"
)

initialCommands in console := "import net.semeai.go._"
