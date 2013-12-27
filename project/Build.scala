import sbt._
import Keys._

object BuildSettings {

  val buildOrganization = "de.bstudios"
  val buildName = "APIGen"
  val buildVersion = "0.1.0"
  val buildScalaVersion = "2.10.3"

  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := buildOrganization,
    version := buildVersion,
    scalaVersion := buildScalaVersion,
    scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-encoding", "UTF-8"),
    resolvers := Seq(
      Resolver.sonatypeRepo("snapshots")
    ),
    addCompilerPlugin("org.scala-lang.plugins" % "macro-paradise" % "2.0.0-SNAPSHOT" cross CrossVersion.full)
  )
}

object Dependencies {
  val scalaLibrary = "org.scala-lang" % "scala-library" % BuildSettings.buildScalaVersion % "provided"
  val scalaReflect = "org.scala-lang" % "scala-reflect" % BuildSettings.buildScalaVersion
}

object APIGenBuild extends Build {

  import BuildSettings._
  import Dependencies._

  lazy val root = Project(
    "root",
    file("."),
    settings = buildSettings ++ Seq(
      libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _)
    )
  )
}
