ThisBuild / version      := "0.1.0"
ThisBuild / organization := "cn.ac.ios.tis"
ThisBuild / scalaVersion := "2.12.15"

resolvers ++= Seq(
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("releases")
)

lazy val publishSettings = Seq(
  versionScheme := {
    if (version.value.contains("-")) Some("early-semver")
    else Some("semver-spec")
  },
  isSnapshot := version.value.endsWith("-SNAPSHOT"),

  // As of February 2021, all new projects began being provisioned on https://s01.oss.sonatype.org/
  sonatypeCredentialHost := "s01.oss.sonatype.org",
  sonatypeRepository     := "https://s01.oss.sonatype.org/service/local",
  // publishTo should be defined by sbt-sonatype

  homepage             := Some(url("https://github.com/iscas-tis/riscv-spec-core")),
  organizationHomepage := Some(url("https://tis.ios.ac.cn")),
  licenses             := List(License.Apache2),
  developers := List(
    Developer("liuyic00", "Yicheng Liu", "liuyic00@gmail.com", url("https://github.com/liuyic00")),
    Developer("SeddonShen", "Shidong Shen", "seddonshen2001@gmail.com", url("https://github.com/SeddonShen"))
  )
)

lazy val root = (project in file("."))
  .settings(publishSettings: _*)
  .settings(
    name := "RiscvSpecCore",
    libraryDependencies ++= Seq(
      "edu.berkeley.cs" %% "chisel3"    % "3.5.4",
      "edu.berkeley.cs" %% "chiseltest" % "0.5.4" % "test"
    ),
    scalacOptions ++= Seq(
      "-Xsource:2.11",
      "-language:reflectiveCalls",
      "-deprecation",
      "-feature",
      "-Xcheckinit"
      // Enables autoclonetype2 in 3.4.x (on by default in 3.5)
      // "-P:chiselplugin:useBundlePlugin"
    ),
    addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % "3.5.4" cross CrossVersion.full),
    addCompilerPlugin("org.scalamacros" % "paradise"       % "2.1.1" cross CrossVersion.full)
  )
