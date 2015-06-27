lazy val commonSettings = Seq(
  organization := "org.labrad",
  version := "0.5.2",
  licenses += ("GPL-2.0", url("http://www.gnu.org/licenses/gpl-2.0.html")),

  scalaVersion := "2.11.7",
  scalacOptions ++= Seq(
    "-deprecation",
    "-feature"
  ),
  javacOptions ++= Seq("-source", "1.7", "-target", "1.7"),

  resolvers ++= Seq(
    Resolver.sonatypeRepo("snapshots"),
    Resolver.sonatypeRepo("releases"),
    "bintray" at "http://jcenter.bintray.com",
    "bintray-maffoo" at "http://dl.bintray.com/maffoo/maven"
  ),

  // eclipse project file generation
  EclipseKeys.withSource := true,
  EclipseKeys.eclipseOutput := Some("target/eclipseOutput"),

  // use bintray to publish library jars
  bintrayOrganization := Some("labrad"),
  bintrayReleaseOnPublish in ThisBuild := false
)

lazy val core = project.in(file("core"))
  .settings(commonSettings)
  .settings(
    name := "scalabrad-core",
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1",
      "org.clapper" %% "argot" % "1.0.3",
      "io.netty" % "netty-all" % "4.1.0.Beta5",
      "joda-time" % "joda-time" % "2.1",
      "org.joda" % "joda-convert" % "1.2",
      "org.slf4j" % "slf4j-api" % "1.7.2",
      "ch.qos.logback" % "logback-classic" % "1.0.6",
      "org.bouncycastle" % "bcprov-jdk15on" % "1.52",
      "org.bouncycastle" % "bcpkix-jdk15on" % "1.52",

      "org.scalatest" %% "scalatest" % "2.2.4" % "test"
    )
  )

lazy val manager = project.in(file("manager"))
  .dependsOn(core % "compile->compile; test->test")
  .settings(commonSettings)
  .settings(
    name := "scalabrad-manager",

    libraryDependencies ++= Seq(
      "com.typesafe.play" %% "anorm" % "2.4.0",
      "org.xerial" % "sqlite-jdbc" % "3.8.7"
    ),
    fork in Test := true,
    parallelExecution in Test := false,

    // When running, connect std in and tell manager to stop on EOF (ctrl+D).
    // This allows us to stop the manager without using ctrl+C, which kills sbt.
    fork in run := true,
    connectInput in run := true,
    javaOptions += "-Dorg.labrad.stopOnEOF=true",

    // use sbt-pack to create distributable package
    packSettings,

    packMain := Map(
      "labrad" -> "org.labrad.manager.Manager",
      "labrad-migrate-registry" -> "org.labrad.registry.Migrate",
      "labrad-sql-test" -> "org.labrad.registry.SQLTest"
    ),

    packGenerateWindowsBatFile := true
  )
