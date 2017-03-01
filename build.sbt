name := "functional-sandbox"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "co.fs2"        %% "fs2-core" % "0.9.2",
  "co.fs2"        %% "fs2-io"   % "0.9.2",
  "org.typelevel" %% "cats"     % "0.8.1",
  "org.scalaz" %% "scalaz-core" % "7.2.5"
)
