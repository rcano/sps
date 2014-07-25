name := "scala-とりまとめる"

scalaVersion := "2.11.1"

fork := true

resolvers += "Local maven repo" at "file://" + Path.userHome + "/.m2/repository/"

resolvers += Resolver.sonatypeRepo("releases")

resolvers += Resolver.sonatypeRepo("snapshots")

packageOptions in (Compile, packageBin) += Package.MainClass("dota.ui.JfxWrapperMain")

mainClass in Compile := Some("dota.ui.JfxWrapperMain")

//addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.0-M8" cross CrossVersion.full)

scalacOptions ++= Seq("-Yinfer-argument-types", "-feature", "-deprecation")

libraryDependencies ++= Seq(
  //"uy.com.netlabs" %% "luthier-core" % "2.0.0-SNAPSHOT",
  //"uy.com.netlabs" %% "luthier-endpoint-logical" % "2.0.0-SNAPSHOT",
  //"uy.com.netlabs" %% "luthier-endpoint-http" % "2.0.0-SNAPSHOT",
  //"uy.com.netlabs" %% "luthier-endpoint-jdbc" % "2.0.0-SNAPSHOT",
  //"uy.com.netlabs" %% "luthier-endpoint-stream" % "2.0.0-SNAPSHOT",
  "org.slf4j" % "slf4j-jdk14" % "1.6.2",
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  //"org.scalamacros" %% "paradise" % "2.0.0-M8" cross CrossVersion.full,
  "org.parboiled" %% "parboiled" % "2.0.0",
  "org.scalatest" %% "scalatest" % "2.2.0" % "test"
)

nbsettings
