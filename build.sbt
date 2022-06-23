import sbtrelease.ReleaseStateTransformations._

inThisBuild {
  val scala213 = "2.13.8"
  val scala3   = "3.1.3"

  List(
    scalaVersion       := scala3,
    crossScalaVersions := Seq(scala213, scala3)
  )
}

def releaseSettings: Seq[Def.Setting[_]] =
  Seq(
    versionScheme               := Some("early-semver"),
    releaseIgnoreUntrackedFiles := true,
    releaseTagName              := s"${version.value}",
    releaseProcess := Seq[ReleaseStep](
      checkSnapshotDependencies,
      inquireVersions,
      runClean,
      runTest,
      setReleaseVersion,
      commitReleaseVersion,
      tagRelease,
      setNextVersion,
      commitNextVersion,
      pushChanges
    ),
    publishTo := None,
    publish   := (())
  )

lazy val root =
  project
    .in(file("."))
    .settings(publish / skip := true)
    .aggregate(core, circe)

lazy val core =
  project
    .in(file("core"))
    .settings(releaseSettings: _*)
    .settings(
      organization := "io.kaizensolutions",
      name         := "json-schema-scala",
      libraryDependencies ++= {
        val magnoliaForScala2     = "com.softwaremill.magnolia1_2" %% "magnolia"      % "1.1.2"
        val scalaReflectForScala2 = "org.scala-lang"                % "scala-reflect" % scalaVersion.value
        val magnoliaForScala3     = "com.softwaremill.magnolia1_3" %% "magnolia"      % "1.1.4"

        if (scalaVersion.value.startsWith("2")) Seq(magnoliaForScala2, scalaReflectForScala2)
        else Seq(magnoliaForScala3)
      }
    )

lazy val circe =
  project
    .in(file("circe"))
    .dependsOn(core)
    .settings(releaseSettings: _*)
    .settings(
      organization := "io.kaizensolutions",
      name         := "json-schema-scala-circe",
      libraryDependencies ++= {
        val circe  = "io.circe"
        val circeV = "0.14.2"
        Seq(
          circe %% "circe-core"    % circeV,
          circe %% "circe-generic" % circeV % Test
        )
      }
    )
