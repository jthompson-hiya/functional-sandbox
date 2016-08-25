name := "functional-sandbox"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "co.fs2"        %% "fs2-core" % "0.9.0-RC2",
  "co.fs2"        %% "fs2-io"   % "0.9.0-RC2",
  "org.typelevel" %% "cats"     % "0.7.0",
  "org.scalaz" %% "scalaz-core" % "7.2.5"
)
