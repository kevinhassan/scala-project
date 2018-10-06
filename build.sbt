import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.polytech",
      scalaVersion := "2.12.5",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "Battleship",
    libraryDependencies ++= Seq(
      scalaTest % Test,
      "com.github.tototoshi" %% "scala-csv" % "1.3.5"
    )
  )