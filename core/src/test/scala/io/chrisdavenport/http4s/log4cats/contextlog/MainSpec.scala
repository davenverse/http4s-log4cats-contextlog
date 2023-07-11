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

  test("Successfully Create a Server Context Log") {
    val logger = StructuredTestingLogger.impl[IO]()

    val server = HttpRoutes.of[IO]{
      case _ =>  Response(Status.Ok).pure[IO]
    }.orNotFound

    val builder = ServerMiddleware.fromLogger(logger)
      .withLogRequestBody(false)
      .withLogResponseBody(false)
      .withRemovedContextKeys(Set("http.duration_ms", "http.access_time"))
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

              "http.target" -> "/",
              "http.exit_case" -> "succeeded",
              "http.method" -> "GET",

              "http.status_code" -> "200",

              "http.host" -> "localhost",
              "http.flavor" -> "1.1",
              "http.url" -> "/",
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
      .withLogRequestBody(true)
      .withLogResponseBody(true)
      .withLogMessage(logMessage)
      .withRemovedContextKeys(Set("http.duration_ms", "http.duration_body_ms", "http.access_time"))

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
              "http.response.header.content-length" -> "20",
              "http.target" -> "/",
              "http.exit_case" -> "succeeded",
              "http.method" -> "GET",
              "http.request_content_length" -> "19",
              "http.status_code" -> "200",
              "http.request.body" -> "Hello from Request!",
              "http.response.body" -> "Hello from Response!",
              "http.kind" -> "server",
              "http.request.header.content-length" -> "19",
              "http.request.header.content-type" -> "text/plain; charset=UTF-8",
              "http.response.header.content-type" -> "text/plain; charset=UTF-8",
              "http.response_content_length" -> "20",
              "http.host" -> "localhost",
              "http.flavor" -> "1.1",
              "http.url" -> "/"
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
      .withLogRequestBody(true)
      .withLogResponseBody(true)
      .withRemovedContextKeys(Set("http.duration_ms", "http.duration_body_ms", "http.access_time"))
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
              "http.response.header.content-length" -> "20",
              "http.target" -> "/",
              "http.exit_case" -> "succeeded",
              "http.method" -> "GET",
              "http.request_content_length" -> "19",
              "http.status_code" -> "200",
              "http.request.body" -> "Hello from Request!",
              "http.response.body" -> "Hello from Response!",
              "http.kind" -> "server",
              "http.request.header.content-length" -> "19",
              "http.request.header.content-type" -> "text/plain; charset=UTF-8",
              "http.response.header.content-type" -> "text/plain; charset=UTF-8",
              "http.response_content_length" -> "20",
              "http.host" -> "localhost",
              "http.flavor" -> "1.1",
              "http.url" -> "/"
            )
          )
        ))
    }
  }

    test("Successfully Create a Client Context Log with Body") {
    val logger = StructuredTestingLogger.impl[IO]()

    val server = HttpRoutes.of[IO]{
      case req => req.body.compile.drain >> Response[IO](Status.Ok).withEntity("Hello from Response!").pure[IO]
    }.orNotFound

    val client = Client.fromHttpApp(server)

    val builder = ClientMiddleware.fromLogger(logger)
      .withLogRequestBody(true)
      .withLogResponseBody(true)
      // .withLogMessage(logMessage)
      .withRemovedContextKeys(Set("http.duration_ms", "http.duration_body_ms", "http.access_time"))

    val finalApp = builder.client(client)
    val request = Request[IO](Method.GET, uri"http://test.http4s.org/").withEntity("Hello from Request!")

    (finalApp.run(request).use(_.body.compile.drain) *> logger.logged).map{
      logged =>
      assertEquals(
        logged,
        Vector(
          INFO(
            "HttpClient \"GET http://test.http4s.org/ HTTP/1.1\" 200 20",
            None,
            Map(
              "http.scheme" -> "http",
              "http.response.header.content-length" -> "20",
              "http.target" -> "/",
              "http.exit_case" -> "succeeded",
              "http.method" -> "GET",
              "http.request_content_length" -> "19",
              "http.status_code" -> "200",
              "http.kind" -> "client",
              "http.request.body" -> "Hello from Request!",
              "net.peer.name" -> "test.http4s.org",
              "http.response.body" -> "Hello from Response!",
              "http.request.header.content-length" -> "19",
              "http.request.header.content-type" -> "text/plain; charset=UTF-8",
              "http.response.header.content-type" -> "text/plain; charset=UTF-8",
              "http.response_content_length" -> "20",
              "http.host" -> "test.http4s.org",
              "http.flavor" -> "1.1",
              "http.url" -> "http://test.http4s.org/"
            )
          )
        ))
    }
  }


}
