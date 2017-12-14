name := "punkt0"

version := "1.0"

scalaVersion := "2.12.3"

scalacOptions ++= Seq("-deprecation", "-unchecked")
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" //version changed as these the only versions supported by 2.12
libraryDependencies += "org.apache.bcel" % "bcel" % "6.2"

