name := "tetris"

version := "1.0"

scalaVersion := "2.12.1"

libraryDependencies += "org.scalafx" %% "scalafx" % "8.0.102-R11"

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

libraryDependencies += "org.scalafx" %% "scalafxml-core-sfx8" % "0.3"

libraryDependencies += "org.specs2" %% "specs2-core" % "3.8.6" % "test"

scalacOptions in Test ++= Seq("-Yrangepos")