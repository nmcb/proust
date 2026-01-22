lazy val root =
  project.in(file("."))
    .settings( name := "proust"
             , version := "0.1.0"
             , scalaVersion := "3.8.1"
             , libraryDependencies += "com.github.sbt" % "junit-interface" % "0.13.3" % "test"
             , scalacOptions := List("-encoding", "utf8", "-Xfatal-warnings", "-deprecation", "-unchecked"),
             Test / classLoaderLayeringStrategy := ClassLoaderLayeringStrategy.Flat
             )
