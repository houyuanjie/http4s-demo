scalaVersion := "3.1.2"

name    := "http4s-demo"
version := "0.0.1"

val Http4sVersion = "1.0.0-M33"
val CirceVersion = "0.14.2"

libraryDependencies ++= Seq(
  "org.http4s" %% "http4s-blaze-server" % Http4sVersion,
  "org.http4s" %% "http4s-circe"        % Http4sVersion,
  "org.http4s" %% "http4s-dsl"          % Http4sVersion,
  "io.circe"   %% "circe-generic"       % CirceVersion
)
