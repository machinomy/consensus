import sbtrelease.ReleaseStateTransformations._
import sbtrelease.ReleasePlugin.autoImport.ReleaseKeys._
import sbtrelease.Version

name := "consensus"

scalaVersion := "2.11.8"

organization := "com.machinomy"

libraryDependencies ++= Seq(
  "org.scodec" %% "scodec-core" % "1.9.0",
  "org.slf4j" % "slf4j-simple" % "1.7.21",
  "org.scalatest" %% "scalatest" % "2.2.6" % "test",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0",
  "com.github.nscala-time" %% "nscala-time" % "2.10.0"
)

def doIfNotSnapshot(step: ReleaseStep) = {
  ReleaseStep(
    action = st => {
      if (!st.get(versions).getOrElse((None, None))._1.toString.endsWith("-SNAPSHOT")) {
        step.action(st)
      } else {
        st
      }
    },
    check = step.check,
    enableCrossBuild = step.enableCrossBuild
  )
}

releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runClean,
  runTest,
  doIfNotSnapshot(setReleaseVersion),
  doIfNotSnapshot(commitReleaseVersion),
  doIfNotSnapshot(tagRelease),
  publishArtifacts,
  doIfNotSnapshot(setNextVersion),
  doIfNotSnapshot(commitNextVersion),
  doIfNotSnapshot(pushChanges)
)

publishTo := {
  if (isSnapshot.value)
    Some("Machinomy" at "http://machinomy.com:8081/artifactory/libs-snapshot-local;build.timestamp=" + new java.util.Date().getTime)
  else
    Some("Machinomy" at "http://machinomy.com:8081/artifactory/libs-release-local/")
}
credentials += Credentials(new File("credentials.properties"))
