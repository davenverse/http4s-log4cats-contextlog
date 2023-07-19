# http4s-log4cats-contextlog - Logging Middlewares for Http4s [![Maven Central](https://maven-badges.herokuapp.com/maven-central/io.chrisdavenport/http4s-log4cats-contextlog_2.13/badge.svg)](https://maven-badges.herokuapp.com/maven-central/io.chrisdavenport/http4s-log4cats-contextlog_2.13) ![Code of Conduct](https://img.shields.io/badge/Code%20of%20Conduct-Scala-blue.svg)

## Intention of Upstreaming

The goal of this library is to solidify the interface and contribute it to http4s. Any contributions should be made with an understanding that this code will eventually be PR'd to http4s, and altered there in accordance with the http4s license.

## Quick Start

To use http4s-log4cats in an existing SBT project with Scala 2.13 or a later version, add the following dependencies to your
`build.sbt` depending on your needs:

```scala
libraryDependencies ++= Seq(
  "io.chrisdavenport" %% "http4s-log4cats-contextlog" % "<version>"
)
```

So lets drop this into your application.

```scala
import org.http4s.{HttpApp, HttpRoutes}
import org.http4s.client.Client
import org.typelevel.log4cats.StructuredLogger

val yourHttpApp: HttpApp[F] = ???
val yourHttpRoutes: HttpRoutes[F] = ???
val yourClient: Client[F] = ???
val yourLoger: StructuredLogger[F] = ???

import io.chrisdavenport.http4s.log4cats.contextlog.{
  ClientMiddleware,
  ServerMiddleware
}

// This client will automatically log for you
val loggedClient: Client[F] = ClientMiddleware.fromLogger(yourLogger)
  .client(client)

val loggedApp = ServerMiddleware.fromLogger(yourLogger)
  .httpApp(yourHttpApp)

// Does not know status or body for unmatched requests
val loggedRoutes = ServerMiddleware.fromLogger(yourLogger)
  .httpRoutes(yourHttpRoutes)
```

The defaults will log normal interactions as `Debug`, 4xx excluding 404 as `Warn`, and 5xx as `Error`.