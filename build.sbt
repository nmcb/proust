lazy val root =
  project.in(file("."))
    .settings( name := "proust"
             , version := "0.1.0"
             , scalaVersion := "3.8.2"
             , libraryDependencies += "com.github.sbt" % "junit-interface" % "0.13.3" % "test"
             , scalacOptions := List("-encoding", "utf8", "-Werror", "-deprecation", "-unchecked"),
             Test / classLoaderLayeringStrategy := ClassLoaderLayeringStrategy.Flat
             )
