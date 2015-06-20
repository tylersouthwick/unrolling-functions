lazy val commonSettings = Seq(
  version := "1.0",
  scalaVersion := "2.11.6"
)
lazy val macros = (project in file("macros")).
  settings(commonSettings : _*).
  settings(
    name := "UnrollingFunctions-macros",
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
  )
lazy val tests = (project in file("tests")).
  settings(commonSettings : _*).
  settings(
    name := "UnrollingFunctions-tests",
    libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"
  ).
  dependsOn(macros)

lazy val root = (project in file(".")).aggregate(tests, macros)
