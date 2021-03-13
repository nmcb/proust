val dottyVersion = "3.0.0-RC1"

lazy val root =
  project.in(file("."))
    .settings( name := "proust"
             , version := "0.1.0"
             , scalaVersion := dottyVersion
             , libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
             )
