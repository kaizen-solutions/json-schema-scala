inThisBuild {
  val scala212 = "2.12.18"
  val scala213 = "2.13.11"
  val scala3   = "3.3.0"

  List(
    scalaVersion       := scala213,
    crossScalaVersions := Seq(scala212, scala213, scala3),
    organization       := "io.kaizen-solutions",
    organizationName   := "kaizen-solutions"
  )
}

lazy val root =
  project
    .in(file("."))
    .settings(publish / skip := true)
    .aggregate(core, circe)

lazy val core =
  project
    .in(file("core"))
    .settings(
      name := "json-schema-scala",
      libraryDependencies ++= {
        val magnoliaForScala2     = "com.softwaremill.magnolia1_2" %% "magnolia"      % "1.1.3"
        val scalaReflectForScala2 = "org.scala-lang"                % "scala-reflect" % scalaVersion.value
        val magnoliaForScala3     = "com.softwaremill.magnolia1_3" %% "magnolia"      % "1.3.1"

        if (scalaVersion.value.startsWith("2")) Seq(magnoliaForScala2, scalaReflectForScala2)
        else Seq(magnoliaForScala3)
      }
    )

lazy val circe =
  project
    .in(file("circe"))
    .dependsOn(core)
    .settings(
      name := "json-schema-scala-circe",
      libraryDependencies ++= {
        val circe  = "io.circe"
        val circeV = "0.14.5"
        Seq(
          circe %% "circe-core"    % circeV,
          circe %% "circe-generic" % circeV % Test
        )
      }
    )
