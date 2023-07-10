package io.chrisdavenport.http4s.log4cats.contextlog

import org.typelevel.ci.CIString
import org.http4s._
import org.http4s.headers._

import cats._
import cats.syntax.all._
import cats.effect._
import cats.effect.syntax.all._
import cats.data.{Kleisli, OptionT}
import org.typelevel.log4cats.{StructuredLogger, SelfAwareStructuredLogger}
import org.typelevel.log4cats.extras.LogLevel
import org.http4s.client.RequestKey
import cats.effect.kernel.Outcome.Canceled
import cats.effect.kernel.Outcome.Errored
import org.typelevel.vault.Key
import scala.concurrent.duration.FiniteDuration
import org.typelevel.log4cats.LoggerFactory
import fs2.{Stream, Pure}
import org.http4s.client.middleware.Retry
import SharedStructuredLogging._
import org.http4s.client.Client
import java.time.ZoneId

object ClientMiddleware {

  object Defaults {
    def willLog[F[_]: Applicative](prelude: Request[Pure]): F[Boolean] = true.pure[F]
    def routeClassifier(prelude: Request[Pure]): Option[String] = None
    def reqHeaders = HttpStructuredContext.Headers.defaultHeadersAllowed
    def requestAdditionalContext(prelude: Request[Pure]) = Map.empty[String, String]
    def requestIncludeUrl(prelude: Request[Pure]) = true
    val requestLogBody = true
    val requestBodyMaxSize = 65535

    def respHeaders = HttpStructuredContext.Headers.defaultHeadersAllowed
    def responseAdditionalContext(prelude: Response[Pure]) = Map.empty[String, String]
    val responseLogBody = true
    val responseBodyMaxSize = 65535
    val removedContextKeys = Set.empty[String]
    def logLevel(prelude: Request[Pure], outcome: Outcome[Option, Throwable, Response[Pure]]): Option[LogLevel] = LogLevel.Info.some
    def logMessage(prelude: Request[Pure], outcome: Outcome[Option, Throwable, Response[Pure]], now: FiniteDuration): String =
      CommonLog.logMessage(ZoneId.systemDefault())(prelude, outcome, now)
  }

  def fromLoggerFactory[F[_]: Concurrent: Clock: LoggerFactory]: ClientMiddlewareBuilder[F] =
    fromLogger(LoggerFactory[F].getLogger)

  def fromLogger[F[_]: Concurrent: Clock](logger: SelfAwareStructuredLogger[F]): ClientMiddlewareBuilder[F] =
    new ClientMiddlewareBuilder[F](
      logger,
      Defaults.willLog[F],
      Defaults.routeClassifier(_),
      Defaults.reqHeaders,
      Defaults.requestAdditionalContext(_),
      Defaults.requestIncludeUrl(_),
      Defaults.responseLogBody,
      Defaults.requestBodyMaxSize,
      Defaults.respHeaders,
      Defaults.responseAdditionalContext(_),
      Defaults.responseLogBody,
      Defaults.responseBodyMaxSize,
      Defaults.removedContextKeys,
      Defaults.logLevel(_, _),
      Defaults.logMessage(_,_,_)
    )

  final class ClientMiddlewareBuilder[F[_]: Concurrent: Clock] private[ClientMiddleware](
    logger: SelfAwareStructuredLogger[F],
    willLog: Request[Pure] => F[Boolean],

    routeClassifier: Request[Pure] => Option[String],

    reqHeaders: Set[CIString],
    requestAdditionalContext: Request[Pure] => Map[String, String],
    requestIncludeUrl: Request[Pure] => Boolean,
    requestLogBody: Boolean,
    requestBodyMaxSize: Long,

    respHeaders: Set[CIString],
    responseAdditionalContext: Response[Pure] => Map[String, String],
    responseLogBody: Boolean,
    responseBodyMaxSize: Long,

    removedContextKeys: Set[String],

    logLevel: (Request[Pure], Outcome[Option, Throwable, Response[Pure]]) => Option[LogLevel],
    logMessage: (Request[Pure], Outcome[Option, Throwable, Response[Pure]], FiniteDuration) => String,
  ){ self =>

    private def copy(
      logger: SelfAwareStructuredLogger[F] = self.logger,
      willLog: Request[Pure] => F[Boolean] = self.willLog,
      routeClassifier: Request[Pure] => Option[String] = self.routeClassifier,
      reqHeaders: Set[CIString] = self.reqHeaders,
      requestAdditionalContext: Request[Pure] => Map[String, String] = self.requestAdditionalContext,
      requestIncludeUrl: Request[Pure] => Boolean = self.requestIncludeUrl,
      requestLogBody: Boolean = self.requestLogBody,
      requestBodyMaxSize: Long = self.requestBodyMaxSize,
      respHeaders: Set[CIString] = self.respHeaders,
      responseAdditionalContext: Response[Pure] => Map[String, String] = self.responseAdditionalContext,
      responseLogBody: Boolean = self.responseLogBody,
      responseBodyMaxSize: Long = self.responseBodyMaxSize,
      removedContextKeys: Set[String] = self.removedContextKeys,
      logLevel: (Request[Pure], Outcome[Option, Throwable, Response[Pure]]) => Option[LogLevel] = self.logLevel,
      logMessage: (Request[Pure], Outcome[Option, Throwable, Response[Pure]], FiniteDuration) => String = self.logMessage,
    ) = new ClientMiddlewareBuilder[F](
      logger,
      willLog,
      routeClassifier,
      reqHeaders,
      requestAdditionalContext,
      requestIncludeUrl,
      requestLogBody,
      requestBodyMaxSize,
      respHeaders,
      responseAdditionalContext,
      responseLogBody,
      responseBodyMaxSize,
      removedContextKeys,
      logLevel,
      logMessage
    )

    def withRemovedContextKeys(removedContextKeys: Set[String]) =
      copy(removedContextKeys = removedContextKeys)

    def withWillLog(willLog: Request[Pure] => F[Boolean]) =
      copy(willLog = willLog)

    def withRouteClassifier(routeClassifier: Request[Pure] => Option[String]) =
      copy(routeClassifier = routeClassifier)
    def withIncludeUrl(includeUrl: Request[Pure] => Boolean) =
      copy(requestIncludeUrl = includeUrl)
    def withAdditionalRequestContext(requestAdditionalContext: Request[Pure] => Map[String, String]) =
      copy(requestAdditionalContext = requestAdditionalContext)
    def withAdditionalResponseContext(responseAdditionalContext: Response[Pure] => Map[String, String]) =
      copy(responseAdditionalContext = responseAdditionalContext)

    def withLogRequestBody(boolean: Boolean) =
      copy(requestLogBody = boolean)
    def withLogResponseBody(boolean: Boolean) =
      copy(responseLogBody = boolean)

    def withRequestBodyMaxSize(l: Long) =
      copy(requestBodyMaxSize = l)
    def withResponseBodyMaxSize(l: Long) =
      copy(responseBodyMaxSize = l)

    def withLogLevel(logLevel: (Request[Pure], Outcome[Option, Throwable, Response[Pure]]) => Option[LogLevel]) =
      copy(logLevel = logLevel)
    def withLogMessage(logMessage: (Request[Pure], Outcome[Option, Throwable, Response[Pure]], FiniteDuration) => String) =
      copy(logMessage = logMessage)

    def withAllowedRequestHeaders(reqHeaders: Set[CIString]) =
      copy(reqHeaders = reqHeaders)
    def withAllowedResponseHeaders(respHeaders: Set[CIString]) =
      copy(respHeaders = respHeaders)

    def client(client: Client[F]): Client[F] =
      if (requestLogBody || responseLogBody) clientWithBody[F](logger, willLog, routeClassifier, reqHeaders, requestAdditionalContext, requestIncludeUrl, requestLogBody, requestBodyMaxSize, respHeaders, responseAdditionalContext, responseLogBody, responseBodyMaxSize, removedContextKeys, logLevel, logMessage)(client)
      else clientNoBody[F](logger, willLog, routeClassifier, reqHeaders, requestAdditionalContext, requestIncludeUrl, respHeaders, responseAdditionalContext, removedContextKeys, logLevel, logMessage)(client)

  }


  private def clientWithBody[F[_]: Concurrent: Clock](
    logger: SelfAwareStructuredLogger[F],
    willLog: Request[Pure] => F[Boolean],

    routeClassifier: Request[Pure] => Option[String],

    reqHeaders: Set[CIString],
    requestAdditionalContext: Request[Pure] => Map[String, String],
    requestIncludeUrl: Request[Pure] => Boolean,
    requestLogBody: Boolean,
    requestBodyMaxSize: Long,

    respHeaders: Set[CIString],
    responseAdditionalContext: Response[Pure] => Map[String, String],
    responseLogBody: Boolean,
    responseBodyMaxSize: Long,

    removedContextKeys: Set[String],
    logLevel: (Request[Pure], Outcome[Option, Throwable, Response[Pure]]) => Option[LogLevel],
    logMessage: (Request[Pure], Outcome[Option, Throwable, Response[Pure]], FiniteDuration) => String,
  )(client: Client[F]): Client[F] = Client{(req: Request[F]) =>
    val pureReq: Request[Pure] = pureRequest(req)
    Resource.eval(willLog(pureReq)).flatMap{ enabled =>
      if (!enabled) client.run(req)
      else {
        Resource.eval(Clock[F].realTime).flatMap{ start =>
          val reqContext = request(pureReq, reqHeaders, routeClassifier, requestIncludeUrl, requestAdditionalContext)
          Concurrent[Resource[F, *]].uncancelable(poll =>
            poll{
              for {
                reqBody <- Resource.eval(Concurrent[F].ref(Option.empty[fs2.Chunk[Byte]]))
                reqPipe = {(s: fs2.Stream[F, Byte]) => s.chunks.evalMap(chunk => reqBody.update{
                  case Some(current) => (current ++ chunk).some
                  case None => chunk.some
                }).drain}
                newReq = {
                  if (requestLogBody && req.contentLength.exists(l => l <= requestBodyMaxSize)){
                    req.withBodyStream(req.body.observe(reqPipe))
                  } else req
                }
                respBody <- Resource.eval(Concurrent[F].ref(Option.empty[fs2.Chunk[Byte]]))
                resp <- client.run{
                  newReq
                }
              } yield {
                  resp.withBodyStream(
                    resp.body.observe((s: fs2.Stream[F, Byte]) =>
                      if (responseLogBody && resp.contentLength.exists(l => l <= responseBodyMaxSize)) {
                        s.chunks.evalMap(chunk => respBody.update{
                          case Some(current) => (current ++ chunk).some
                          case None => chunk.some
                        }).drain
                      } else s.drain
                    )
                      .onFinalizeWeak{
                        val pureResp = pureResponse(resp)
                        for {
                          end <- Clock[F].realTime
                          reqBodyFinal <- reqBody.get
                          reqBodyS <- reqBodyFinal.traverse(chunk => logBody(req.withBodyStream(fs2.Stream.chunk(chunk))))
                          bodyPureReq= reqBodyFinal.fold(pureReq)(chunk => pureReq.withBodyStream(Stream.chunk(chunk)))
                          reqContext = request(bodyPureReq, reqHeaders, routeClassifier, requestIncludeUrl, requestAdditionalContext)
                          respBodyFinal <- respBody.get
                          respBodyS <- respBodyFinal.traverse(chunk => logBody(resp.withBodyStream(fs2.Stream.chunk(chunk))))
                          duration = "http.duration_ms" -> end.minus(start).toMillis.toString()
                          requestBodyCtx = reqBodyS.map(body => Map("http.request.body" -> body)).getOrElse(Map.empty)
                          responseCtx = response(respBodyFinal.fold(pureResp)(body => pureResp.withBodyStream(Stream.chunk(body))), respHeaders, responseAdditionalContext)
                          responseBodyCtx = respBodyS.map(body => Map("http.response.body" -> body)).getOrElse(Map.empty)
                          outcome = Outcome.succeeded[Option, Throwable, Response[Pure]](respBodyFinal.fold(pureResp)(body => pureResp.withBodyStream(Stream.chunk(body))).some)
                          outcomeCtx = outcomeContext(outcome)
                          finalCtx = reqContext ++ responseCtx + outcomeCtx + duration ++ requestBodyCtx ++ responseBodyCtx
                          _ <- logLevelAware(logger, finalCtx, bodyPureReq, outcome, end, removedContextKeys, logLevel, logMessage)
                        } yield ()
                      }
                  )
              }
            }
              .guaranteeCase{
                case Outcome.Canceled() =>
                  Resource.eval(Clock[F].realTime.flatMap{ end =>
                    val duration = "http.duration_ms" -> end.minus(start).toMillis.toString()
                    val outcome = Outcome.canceled[Option, Throwable, Response[Pure]]
                    val outcomeCtx = outcomeContext(outcome)
                    val finalCtx = reqContext + outcomeCtx + duration
                    logLevelAware(logger, finalCtx, pureReq, outcome, end, removedContextKeys, logLevel, logMessage)
                  })
                case Outcome.Errored(e) =>
                  Resource.eval(Clock[F].realTime.flatMap{ end =>
                    val duration = "http.duration_ms" -> end.minus(start).toMillis.toString()
                    val outcome = Outcome.errored[Option, Throwable, Response[Pure]](e)
                    val outcomeCtx = outcomeContext(outcome)
                    val finalCtx = reqContext + outcomeCtx + duration
                    logLevelAware(logger, finalCtx, pureReq, outcome, end, removedContextKeys, logLevel, logMessage)
                  })
                case Outcome.Succeeded(_) =>  Resource.eval(Applicative[F].unit)
              }
          )
        }
      }
    }
  }

  private def clientNoBody[F[_]: Concurrent: Clock](
    logger: SelfAwareStructuredLogger[F],
    willLog: Request[Pure] => F[Boolean],

    routeClassifier: Request[Pure] => Option[String],

    reqHeaders: Set[CIString],
    requestAdditionalContext: Request[Pure] => Map[String, String],
    requestIncludeUrl: Request[Pure] => Boolean,

    respHeaders: Set[CIString],
    responseAdditionalContext: Response[Pure] => Map[String, String],

    removedContextKeys: Set[String],
    logLevel: (Request[Pure], Outcome[Option, Throwable, Response[Pure]]) => Option[LogLevel],
    logMessage: (Request[Pure], Outcome[Option, Throwable, Response[Pure]], FiniteDuration) => String,
  )(client: Client[F]): Client[F] = Client{(req: Request[F]) =>
    val pureReq: Request[Pure] = pureRequest(req)
    Resource.eval(willLog(pureReq)).flatMap{ enabled =>
      if (!enabled) client.run(req)
      else {
        Resource.eval(Clock[F].realTime).flatMap{ start =>
          val reqContext = request(pureReq, reqHeaders, routeClassifier, requestIncludeUrl, requestAdditionalContext)
          Concurrent[Resource[F, *]].uncancelable(poll =>
            poll(client.run(req))
              .guaranteeCase{
                case Outcome.Canceled() =>
                  Resource.eval(Clock[F].realTime.flatMap{ end =>
                    val duration = "http.duration_ms" -> end.minus(start).toMillis.toString()
                    val outcome = Outcome.canceled[Option, Throwable, Response[Pure]]
                    val outcomeCtx = outcomeContext(outcome)
                    val finalCtx = reqContext + outcomeCtx + duration
                    logLevelAware(logger, finalCtx, pureReq, outcome, end, removedContextKeys, logLevel, logMessage)
                  })
                case Outcome.Errored(e) =>
                  Resource.eval(Clock[F].realTime.flatMap{ end =>
                    val duration = "http.duration_ms" -> end.minus(start).toMillis.toString()
                    val outcome = Outcome.errored[Option, Throwable, Response[Pure]](e)
                    val outcomeCtx = outcomeContext(outcome)
                    val finalCtx = reqContext + outcomeCtx + duration
                    logLevelAware(logger, finalCtx, pureReq, outcome, end, removedContextKeys, logLevel, logMessage)
                  })
                case Outcome.Succeeded(fa) => fa.flatMap{
                  case resp =>
                    val pureResp = pureResponse(resp)
                    Resource.eval(Clock[F].realTime.flatMap{ end =>
                      val duration = "http.duration_ms" -> end.minus(start).toMillis.toString()
                      val responseCtx = response(pureResp, respHeaders, responseAdditionalContext)
                      val outcome = Outcome.succeeded[Option, Throwable, Response[Pure]](pureResp.some)
                      val outcomeCtx = outcomeContext(outcome)
                      val finalCtx = reqContext ++ responseCtx + outcomeCtx + duration
                      logLevelAware(logger, finalCtx, pureReq, outcome, end, removedContextKeys, logLevel, logMessage)
                    })
                }
              }
          )
        }
      }
    }
  }

  private def request[F[_]](request: Request[Pure], headers: Set[CIString], routeClassifier: Request[Pure] => Option[String], includeUrl: Request[Pure] => Boolean, additionalRequestContext: Request[Pure] => Map[String, String]): Map[String, String] = {
    val builder = Map.newBuilder[String, String]
    builder += HttpStructuredContext.Common.method(request.method)
    if (includeUrl(request)) {
      builder += HttpStructuredContext.Common.url(request.uri)
      builder += HttpStructuredContext.Common.target(request.uri)
    }
    val host = request.headers.get[Host].getOrElse{
      val key = RequestKey.fromRequest(request)
      Host(key.authority.host.value, key.authority.port)
    }
    builder += HttpStructuredContext.Common.host(host)
    request.uri.scheme.foreach( s =>
      builder += HttpStructuredContext.Common.scheme(s)
    )
    request.headers.get[`User-Agent`].foreach( ua =>
      builder += HttpStructuredContext.Common.userAgent(ua)
    )

    request.contentLength.foreach(l => 
      builder += HttpStructuredContext.Common.requestContentLength(l)
    )
    routeClassifier(request).foreach(s =>
      builder += HttpStructuredContext.Server.route(s)
    )
    

    builder += HttpStructuredContext.Common.flavor(request.httpVersion)

    request.remote.foreach{sa =>
      builder += 
        HttpStructuredContext.Common.peerIp(sa.host)
      
      builder += 
        HttpStructuredContext.Common.peerPort(sa.port)
    }
    // Special Server
    request.from.foreach(ip =>
      builder += HttpStructuredContext.Server.clientIp(ip)
    )
    builder ++=
      HttpStructuredContext.Headers.request(request.headers, headers)

    builder ++= additionalRequestContext(request)

    retryCount(request.attributes).foreach{ count =>
      builder += HttpStructuredContext.Common.retryCount(count)
    }

    builder.result()
  }

  def response[F[_]](response: Response[Pure], headers: Set[CIString], responseAdditionalContext: Response[Pure] => Map[String, String]): Map[String, String] = {
    val builder = Map.newBuilder[String, String]

    builder += HttpStructuredContext.Common.status(response.status)
    response.contentLength.foreach(l => 
      builder += HttpStructuredContext.Common.responseContentLength(l)
    )
    // Due to negotiation. Only the response knows what protocol was selected
    builder += HttpStructuredContext.Common.flavor(response.httpVersion)
    builder ++=
      HttpStructuredContext.Headers.response(response.headers, headers)

    builder ++= responseAdditionalContext(response)

    builder.result()
  }

  private def retryCount(vault: org.typelevel.vault.Vault): Option[Int] = {
    // AttemptCountKey is 1,2,3,4 for the initial request,
    // since we want to do retries. We substract by 1 to get 0,1,2,3.
    vault.lookup(Retry.AttemptCountKey).map(i => i - 1)
  }

}