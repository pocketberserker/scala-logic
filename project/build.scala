import sbt._
import Keys._
import sbtrelease._
import xerial.sbt.Sonatype._
import ReleaseStateTransformations._
import com.typesafe.sbt.pgp.PgpKeys

object ScalaLogicBuild extends Build {
  import Dependencies._

  private def gitHash: String = scala.util.Try(
    sys.process.Process("git rev-parse HEAD").lines_!.head
  ).getOrElse("master")

  lazy val buildSettings = Seq(
    ReleasePlugin.releaseSettings,
    sonatypeSettings
  ).flatten ++ Seq(
    scalaVersion := "2.11.6",
    crossScalaVersions := Seq("2.10.5", scalaVersion.value),
    resolvers += Opts.resolver.sonatypeReleases,
    scalacOptions ++= (
      "-deprecation" ::
      "-unchecked" ::
      "-Xlint" ::
      "-feature" ::
      "-language:existentials" ::
      "-language:higherKinds" ::
      "-language:implicitConversions" ::
      "-language:reflectiveCalls" ::
      Nil
    ),
    scalacOptions ++= {
      if(scalaVersion.value.startsWith("2.11"))
        Seq("-Ywarn-unused", "-Ywarn-unused-import")
      else
        Nil
    },
    libraryDependencies ++= Seq(
      scalaz,
      scalacheck % "test",
      scalazScalaCheckBinding % "test"
    ),
    resolvers += "bintray/non" at "http://dl.bintray.com/non/maven",
    addCompilerPlugin("org.spire-math" % "kind-projector" % "0.5.2"  cross CrossVersion.binary),
    ReleasePlugin.ReleaseKeys.releaseProcess := Seq[ReleaseStep](
      checkSnapshotDependencies,
      inquireVersions,
      runClean,
      runTest,
      setReleaseVersion,
      commitReleaseVersion,
      tagRelease,
      ReleaseStep(
        action = state => Project.extract(state).runTask(PgpKeys.publishSigned, state)._1,
        enableCrossBuild = true
      ),
      setNextVersion,
      commitNextVersion,
      pushChanges
    ),
    organization := "com.github.pocketberserker",
    homepage := Some(url("https://github.com/pocketberserker/scala-logic")),
    licenses := Seq("MIT License" -> url("http://www.opensource.org/licenses/mit-license.php"))
  )

  lazy val logic = Project(
    id = "scala-logic",
    base = file("."),
    settings = buildSettings
  )

  object Dependencies {
    val scalazVersion = "7.1.1"
    val scalaz = "org.scalaz" %% "scalaz-core" % scalazVersion
    val scalacheck = "org.scalacheck" %% "scalacheck" % "1.11.4"
    val scalazScalaCheckBinding = "org.scalaz" %% "scalaz-scalacheck-binding" % scalazVersion
  }
}
