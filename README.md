# http4s-log4cats-contextlog - Logging Middlewares for Http4s [![Maven Central](https://maven-badges.herokuapp.com/maven-central/io.chrisdavenport/http4s-log4cats-contextlog_2.13/badge.svg)](https://maven-badges.herokuapp.com/maven-central/io.chrisdavenport/http4s-log4cats-contextlog_2.13) ![Code of Conduct](https://img.shields.io/badge/Code%20of%20Conduct-Scala-blue.svg)


## Quick Start

To use http4s-log4cats in an existing SBT project with Scala 2.13 or a later version, add the following dependencies to your
`build.sbt` depending on your needs:

```scala
libraryDependencies ++= Seq(
  "io.chrisdavenport" %% "http4s-log4cats-contextlog" % "<version>"
)
```


## Intention of Upstreaming

The goal of this library is to solidify the interface and contribute it to http4s. Any contributions should be made with an understanding that this code will eventually be PR'd to http4s, and altered there in accordance with the http4s license.
