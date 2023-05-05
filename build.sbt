ThisBuild / scalaVersion := "2.13.1"
ThisBuild / organization := "com.example"

lazy val root = (project in file("."))
  .settings(
    name := "typeclass-exercises",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.9.0",
      "org.typelevel" %% "cats-laws" % "2.9.0",
      "org.typelevel" %% "discipline-core" % "1.5.1" % Test,
      "org.typelevel" %% "discipline-scalatest" % "2.2.0" % Test,
      "org.scalatest" %% "scalatest" % "3.2.15" % Test
    )
  )
addCompilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full)