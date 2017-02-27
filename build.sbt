name := "entrytodoc"
version := "1.0"
scalaVersion := "2.11.8"
mainClass in assembly := Some("org.vndzzz.entrytodoc.bot.Main")

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies ++= Seq(
  "info.mukel" %% "telegrambot4s" % "2.0.2-SNAPSHOT",
  "io.getquill" %% "quill-async-postgres" % "1.0.1",
  "com.github.cb372" %% "scalacache-guava" % "0.9.3",
  "com.softwaremill.common" %% "tagging" % "1.0.0",
  "com.softwaremill.quicklens" %% "quicklens" % "1.4.8",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:postfixOps",
  "-Ywarn-dead-code",
  "-Ywarn-infer-any",
  "-Ywarn-unused-import",
  "-Xfatal-warnings",
  "-Xlint"
)
