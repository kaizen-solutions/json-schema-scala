import BuildSettings._

name := "json-schema-scala"

version := "0.1"

scalaVersion := scala2Version

lazy val root =
  project
    .in(file("."))
    .settings(publish / skip := true)
    .aggregate(core, circe)

lazy val core =
  project
    .in(file("core"))
    .settings(scalaVersion := scala2Version)
    .settings(scalacOptions ++= scalaCompilerOptions)
    .settings(libraryDependencies += "com.softwaremill.magnolia" %% "magnolia-core" % "1.0.0-M4")

lazy val circe =
  project
    .in(file("circe"))
    .dependsOn(core)
    .settings(scalaVersion := scala2Version)
    .settings(scalacOptions ++= scalaCompilerOptions)
    .settings(
      libraryDependencies ++= {
        val circe  = "io.circe"
        val circeV = "0.14.1"
        Seq(
          circe %% "circe-core"           % circeV,
          circe %% "circe-generic"        % circeV % Test,
          circe %% "circe-generic-extras" % circeV % Test
        )
      }
    )
