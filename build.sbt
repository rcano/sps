name := "sps"

scalaVersion := "2.11.2"

fork := true

resolvers += Resolver.sonatypeRepo("releases")

resolvers += Resolver.sonatypeRepo("snapshots")

scalacOptions ++= Seq("-Yinfer-argument-types", "-feature", "-deprecation")

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "org.parboiled" %% "parboiled" % "2.0.0",
  "org.scalatest" %% "scalatest" % "2.2.0" % "test"
)
