import AssemblyKeys._
assemblySettings

/** Project **/
name := "optimizers"

version := "1.0-SNAPSHOT"

organization := "com.signalcollect"

scalaVersion := "2.11.2"

resolvers += "Typesafe Snapshot Repository" at "http://repo.typesafe.com/typesafe/snapshots/"

resolvers += "Scala-Tools Repository" at "https://oss.sonatype.org/content/groups/scala-tools/"

resolvers += "Sonatype Snapshots Repository" at "https://oss.sonatype.org/content/repositories/snapshots/"

parallelExecution in Test := false

excludedJars in assembly <<= (fullClasspath in assembly) map { cp => 
  cp filter {_.data.getName == "minlog-1.2.jar"}
}

/** Dependencies */
libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-library" % "2.11.2"  % "compile",
  "junit" % "junit" % "4.8.2"  % "test",
  "com.google.collections" % "google-collections" % "1.0",
  "org.specs2" % "classycle" % "1.4.1" % "test",
  "org.mockito" % "mockito-all" % "1.9.0"  % "test",
  "org.scalacheck" %% "scalacheck" % "1.11.0" % "test",
  "org.easymock" % "easymock" % "3.2" % "test",
  "com.typesafe.slick" %% "slick" % "2.1.0",
  "org.slf4j" % "slf4j-nop" % "1.6.4"
)
