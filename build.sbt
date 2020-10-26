val dottyVersion = "0.25.0-RC2"

val hygienicScalacOps: Seq[String] =
  Seq( "-encoding", "utf8"
     , "-language:implicitConversions,higherKinds,existentials,postfixOps"
     , "-deprecation"
     , "-unchecked"
     , "-Xfatal-warnings"
     )

lazy val root =
  project.in(file("."))
    .settings( name := "proust"
             , version := "0.1.0"
             , scalaVersion := dottyVersion
             , libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
             , scalacOptions := hygienicScalacOps
             )
