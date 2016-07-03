import com.typesafe.sbt.SbtStartScript

name := "codeswitch"

version := "0.0.1-SNAPSHOT"

organization := "dhg"

scalaVersion := "2.11.8"

resolvers ++= Seq(
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
  "dhg releases repo" at "http://www.cs.utexas.edu/~dhg/maven-repository/releases",
  "dhg snapshot repo" at "http://www.cs.utexas.edu/~dhg/maven-repository/snapshots"
)

libraryDependencies ++= Seq(
  "dhg" % "scala-util_2.11" % "0.0.2-SNAPSHOT",
  "dhg" % "condor-util_2.11" % "0.0.2-SNAPSHOT",
  "junit" % "junit" % "4.11",
  "com.novocode" % "junit-interface" % "0.10" % "test") //switch to ScalaTest at some point...

seq(SbtStartScript.startScriptForClassesSettings: _*)

scalacOptions ++= Seq("-deprecation")

initialCommands in console := "import dhg.util._, scalaz._, Scalaz._"

