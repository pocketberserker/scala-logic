import sbtrelease.ReleaseStateTransformations._

val scalazVersion = "7.3.4"
val scalaz = "org.scalaz" %% "scalaz-core" % scalazVersion cross CrossVersion.for3Use2_13

def gitHash: String = scala.util.Try(
  sys.process.Process("git rev-parse HEAD").lineStream.head
).getOrElse("master")

val unusedWarnings = Def.setting(
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, 11)) =>
      Seq("-Ywarn-unused-import")
    case _ =>
      Seq("-Ywarn-unused:imports")
  }
)

val Scala211 = "2.11.12"

lazy val buildSettings = Def.settings(
  BuildInfoPlugin.projectSettings,
  scalapropsWithScalaz,
  scalaVersion := Scala211,
  crossScalaVersions := Seq(Scala211, "2.12.14", "2.13.6", "3.0.3-RC1-bin-20210721-ec15557-NIGHTLY"),
  scalacOptions ++= (
    "-deprecation" ::
    "-unchecked" ::
    "-feature" ::
    "-language:existentials" ::
    "-language:higherKinds" ::
    "-language:implicitConversions" ::
    "-language:reflectiveCalls" ::
    Nil
  ),
  scalacOptions ++= unusedWarnings.value,
  scalapropsVersion := "0.8.3",
  publishTo := sonatypePublishTo.value,
  libraryDependencies ++= Seq(
    scalaz
  ),
  scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, _)) =>
        Seq()
      case _ =>
        Seq(
          "-Ykind-projector"
        )
    }
  },
  libraryDependencies ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, _)) =>
        Seq(
          compilerPlugin("org.typelevel" % "kind-projector" % "0.13.0" cross CrossVersion.full)
        )
      case _ =>
        Nil
    }
  },
  buildInfoKeys ++= Seq[BuildInfoKey](
    organization,
    name,
    version,
    scalaVersion,
    sbtVersion,
    scalacOptions,
    licenses,
    "scalazVersion" -> scalazVersion
  ),
  buildInfoPackage := "logic",
  buildInfoObject := "BuildInfoScalaLogic",
  releaseProcess := Seq[ReleaseStep](
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
  credentials ++= PartialFunction.condOpt(sys.env.get("SONATYPE_USER") -> sys.env.get("SONATYPE_PASS")){
    case (Some(user), Some(pass)) =>
      Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", user, pass)
  }.toList,
  organization := "com.github.pocketberserker",
  homepage := Some(url("https://github.com/pocketberserker/scala-logic")),
  licenses := Seq("MIT License" -> url("http://www.opensource.org/licenses/mit-license.php")),
  pomExtra :=
    <developers>
      <developer>
        <id>pocketberserker</id>
        <name>Yuki Nakayama</name>
        <url>https://github.com/pocketberserker</url>
      </developer>
    </developers>
    <scm>
      <url>git@github.com:pocketberserker/scala-logic.git</url>
      <connection>scm:git:git@github.com:pocketberserker/scala-logic.git</connection>
      <tag>{if(isSnapshot.value) gitHash else { "v" + version.value }}</tag>
    </scm>
  ,
  description := "logic programming monad for Scala",
  pomPostProcess := { node =>
    import scala.xml._
    import scala.xml.transform._
    def stripIf(f: Node => Boolean) = new RewriteRule {
      override def transform(n: Node) =
        if (f(n)) NodeSeq.Empty else n
    }
    val stripTestScope = stripIf { n => n.label == "dependency" && (n \ "scope").text == "test" }
    new RuleTransformer(stripTestScope).transform(node)(0)
  },
  Seq(Compile, Test).flatMap(c =>
    (c / console / scalacOptions) --= unusedWarnings.value
  )
)

lazy val logic = Project(
  id = "scala-logic",
  base = file(".")
).settings(
  buildSettings
)
