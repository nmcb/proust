val dottyVersion = "3.6.3"

lazy val root =
  project.in(file("."))
    .settings( name := "proust"
             , version := "0.1.0"
             , scalaVersion := dottyVersion
             , libraryDependencies += "com.github.sbt" % "junit-interface" % "0.13.3" % "test"
             , scalacOptions := List("-encoding", "utf8", "-Xfatal-warnings", "-deprecation", "-unchecked"),
             Test / classLoaderLayeringStrategy := ClassLoaderLayeringStrategy.Flat
             )
