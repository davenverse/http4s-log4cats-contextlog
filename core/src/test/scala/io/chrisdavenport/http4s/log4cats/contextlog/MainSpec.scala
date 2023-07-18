package io.chrisdavenport.http4s.log4cats.contextlog

import munit.CatsEffectSuite
import cats.effect._
import cats.syntax.all._
import org.http4s._
import org.http4s.implicits._
import org.typelevel.log4cats.testing.StructuredTestingLogger
import org.typelevel.log4cats.extras._
import org.typelevel.log4cats.testing.StructuredTestingLogger.TRACE
import org.typelevel.log4cats.testing.StructuredTestingLogger.DEBUG
import org.typelevel.log4cats.testing.StructuredTestingLogger.INFO
import org.typelevel.log4cats.testing.StructuredTestingLogger.WARN
import org.typelevel.log4cats.testing.StructuredTestingLogger.ERROR
import org.http4s.client.Client
import fs2.Pure
import scala.concurrent.duration.FiniteDuration

class MainSpec extends CatsEffectSuite {

  def logMessage(prelude: Request[Pure], outcome: Outcome[Option, Throwable, Response[Pure]], now: FiniteDuration): String = s"Http Server - ${prelude.method}"
  def ignoredKeys(prelude: Request[Pure], outcome: Outcome[Option, Throwable, Response[Pure]]) = {
    Set("http.duration_ms", "http.duration_body_ms", "http.access_time")
  }

  test("Successfully Create a Server Context Log") {
    val logger = StructuredTestingLogger.impl[IO]()

    val server = HttpRoutes.of[IO]{
      case _ =>  Response(Status.Ok).pure[IO]
    }.orNotFound

    val builder = ServerMiddleware.fromLogger(logger)
      .withObserveRequestBody(false)
      .withObserveResponseBody(false)

      .withRemovedContextKeys(ignoredKeys)
      .withLogMessage(logMessage)

    val finalApp = builder.httpApp(server)

    (finalApp.run(Request[IO](Method.GET)) *> logger.logged).map{
      logged =>
      assertEquals(
        logged,
        Vector(
          INFO(
            "Http Server - GET",
            None,
            Map(

              "http.request.target" -> "/",
              "http.exit_case" -> "succeeded",
              "http.request.method" -> "GET",

              "http.response.status_code" -> "200",

              "http.request.host" -> "localhost",
              "http.flavor" -> "1.1",
              "http.request.url" -> "/",
              "http.kind" -> "server",
            )
          )
        ))
    }
  }

  test("Successfully Create a Server Context Log with Body") {
    val logger = StructuredTestingLogger.impl[IO]()

    val server = HttpRoutes.of[IO]{
      case req => req.body.compile.drain >> Response[IO](Status.Ok).withEntity("Hello from Response!").pure[IO]
    }.orNotFound

    val builder = ServerMiddleware.fromLogger(logger)
      .withLogMessage(logMessage)
      .withRemovedContextKeys(ignoredKeys)

    val finalApp = builder.httpApp(server)
    val request = Request[IO](Method.GET).withEntity("Hello from Request!")

    (finalApp.run(request).flatMap(_.body.compile.drain) *> logger.logged).map{
      logged =>
      assertEquals(
        logged,
        Vector(
          INFO(
            "Http Server - GET",
            None,
            Map(
              "http.response.headers.content-length" -> "20",
              "http.request.target" -> "/",
              "http.exit_case" -> "succeeded",
              "http.request.method" -> "GET",
              "http.request.content_length" -> "19",
              "http.response.status_code" -> "200",
              "http.request.body" -> "Hello from Request!",
              "http.response.body" -> "Hello from Response!",
              "http.kind" -> "server",
              "http.request.headers.content-length" -> "19",
              "http.request.headers.content-type" -> "text/plain; charset=UTF-8",
              "http.response.headers.content-type" -> "text/plain; charset=UTF-8",
              "http.response.content_length" -> "20",
              "http.request.host" -> "localhost",
              "http.flavor" -> "1.1",
              "http.request.url" -> "/"
            )
          )
        ))
    }
  }

  test("Log Using the Body") {
    val logger = StructuredTestingLogger.impl[IO]()

    val server = HttpRoutes.of[IO]{
      case req => req.body.compile.drain >> Response[IO](Status.Ok).withEntity("Hello from Response!").pure[IO]
    }.orNotFound

    val builder = ServerMiddleware.fromLogger(logger)
      .withRemovedContextKeys(ignoredKeys)
      .withLogMessage{
        case (req, Outcome.Succeeded(Some(resp)), _) => s"Req Body - ${req.body.through(fs2.text.utf8.decode).compile.string}\nResp Body - ${resp.body.through(fs2.text.utf8.decode).compile.string}"
        case (_, _, _) => "Whoops!"
      }

    val finalApp = builder.httpApp(server)
    val request = Request[IO](Method.GET).withEntity("Hello from Request!")

    (finalApp.run(request).flatMap(_.body.compile.drain) *> logger.logged).map{
      logged =>
      assertEquals(
        logged,
        Vector(
          INFO(
            "Req Body - Hello from Request!\nResp Body - Hello from Response!",
            None,
            Map(
              "http.response.headers.content-length" -> "20",
              "http.request.target" -> "/",
              "http.exit_case" -> "succeeded",
              "http.request.method" -> "GET",
              "http.request.content_length" -> "19",
              "http.response.status_code" -> "200",
              "http.request.body" -> "Hello from Request!",
              "http.response.body" -> "Hello from Response!",
              "http.kind" -> "server",
              "http.request.headers.content-length" -> "19",
              "http.request.headers.content-type" -> "text/plain; charset=UTF-8",
              "http.response.headers.content-type" -> "text/plain; charset=UTF-8",
              "http.response.content_length" -> "20",
              "http.request.host" -> "localhost",
              "http.flavor" -> "1.1",
              "http.request.url" -> "/"
            )
          )
        ))
    }
  }

  test("Successfully Create a Client Context Log with Body") {
    val logger = StructuredTestingLogger.impl[IO]()

    def clientLogMessage(prelude: Request[Pure], outcome: Outcome[Option, Throwable, Response[Pure]], now: FiniteDuration): String =
      s"HttpClient - ${prelude.method}"

    val server = HttpRoutes.of[IO]{
      case req => req.body.compile.drain >> Response[IO](Status.Ok).withEntity("Hello from Response!").pure[IO]
    }.orNotFound

    val client = Client.fromHttpApp(server)

    val builder = ClientMiddleware.fromLogger(logger)
      .withLogMessage(clientLogMessage)
      .withRemovedContextKeys(ignoredKeys)

    val finalApp = builder.client(client)
    val request = Request[IO](Method.GET, uri"http://test.http4s.org/").withEntity("Hello from Request!")

    (finalApp.run(request).use(_.body.compile.drain) *> logger.logged).map{
      logged =>
      assertEquals(
        logged,
        Vector(
          INFO(
            "HttpClient - GET",
            None,
            Map(
              "http.request.method" -> "GET",
              "http.request.scheme" -> "http",
              "http.response.headers.content-length" -> "20",
              "http.request.target" -> "/",
              "http.exit_case" -> "succeeded",

              "http.request.content_length" -> "19",
              "http.response.status_code" -> "200",
              "http.kind" -> "client",
              "http.request.body" -> "Hello from Request!",
              "net.peer.name" -> "test.http4s.org",
              "http.response.body" -> "Hello from Response!",
              "http.request.headers.content-length" -> "19",
              "http.request.headers.content-type" -> "text/plain; charset=UTF-8",
              "http.response.headers.content-type" -> "text/plain; charset=UTF-8",
              "http.response.content_length" -> "20",
              "http.request.host" -> "test.http4s.org",
              "http.flavor" -> "1.1",
              "http.request.url" -> "http://test.http4s.org/"
            )
          )
        ))
    }
  }

  test("Successfully Create a Client Context Log with request body, even if response body is not drained") {
    val logger = StructuredTestingLogger.impl[IO]()

    def clientLogMessage(prelude: Request[Pure], outcome: Outcome[Option, Throwable, Response[Pure]], now: FiniteDuration): String =
      s"HttpClient - ${prelude.method}"

    val server = HttpRoutes.of[IO]{
      case req => req.body.compile.drain >> Response[IO](Status.Ok).withEntity("Hello from Response!").pure[IO]
    }.orNotFound

    val client = Client.fromHttpApp(server)

    val builder = ClientMiddleware.fromLogger(logger)
      .withLogMessage(clientLogMessage)
      .withRemovedContextKeys(ignoredKeys)

    val finalApp = builder.client(client)
    val request = Request[IO](Method.GET, uri"http://test.http4s.org/").withEntity("Hello from Request!")

    (finalApp.status(request) *> logger.logged).map{
      logged =>
      assertEquals(
        logged,
        Vector(
          INFO(
            "HttpClient - GET",
            None,
            Map(
              "http.request.method" -> "GET",
              "http.request.scheme" -> "http",
              "http.response.headers.content-length" -> "20",
              "http.request.target" -> "/",
              "http.exit_case" -> "succeeded",

              "http.request.content_length" -> "19",
              "http.response.status_code" -> "200",
              "http.kind" -> "client",
              "http.request.body" -> "Hello from Request!",
              "net.peer.name" -> "test.http4s.org",
              "http.request.headers.content-length" -> "19",
              "http.request.headers.content-type" -> "text/plain; charset=UTF-8",
              "http.response.headers.content-type" -> "text/plain; charset=UTF-8",
              "http.response.content_length" -> "20",
              "http.request.host" -> "test.http4s.org",
              "http.flavor" -> "1.1",
              "http.request.url" -> "http://test.http4s.org/"
            )
          )
        ))
    }
  }


}
