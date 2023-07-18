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
import SharedStructuredLogging._
import java.time.{Instant, ZoneId}
import java.time.format.DateTimeFormatter
import org.typelevel.ci._

object ServerMiddleware {
  object Defaults {
    def willLog[F[_]: Applicative](prelude: Request[Pure]): F[Boolean] = true.pure[F]
    def routeClassifier(prelude: Request[Pure]): Option[String] = None
    def reqHeaders = HttpStructuredContext.Headers.defaultHeadersAllowed
    def requestIncludeUrl(prelude: Request[Pure]) = true
    val requestObserveBody = true
    def requestBodyEncoder(req: Request[Pure]): Option[String] =
      SharedStructuredLogging.logBody(req).some
    val requestBodyMaxSize = 65535

    def respHeaders = HttpStructuredContext.Headers.defaultHeadersAllowed
    val responseObserveBody = true
    def responseBodyEncoder(resp: Response[Pure]): Option[String] =
      SharedStructuredLogging.logBody(resp).some
    val responseBodyMaxSize = 65535
    def removedContextKeys(request: Request[Pure], outcome: Outcome[Option, Throwable, Response[Pure]]) = Set.empty[String]
    def additionalContext(request: Request[Pure], outcome: Outcome[Option, Throwable, Response[Pure]]): Map[String, String] = Map.empty[String, String]
    def logLevel(request: Request[Pure], outcome: Outcome[Option, Throwable, Response[Pure]]): Option[LogLevel] =
      SharedStructuredLogging.logLevel(request, outcome)
    def quietLogLevel(request: Request[Pure], outcome: Outcome[Option, Throwable, Response[Pure]]): Option[LogLevel] =
      SharedStructuredLogging.quietLogLevel(request, outcome)
    def logMessage(request: Request[Pure], outcome: Outcome[Option, Throwable, Response[Pure]], now: FiniteDuration): String =
      CommonLog.logMessage(ZoneId.systemDefault(), false, false, false)(request, outcome, now)
  }


  def fromLoggerFactory[F[_]: Concurrent: Clock: LoggerFactory]: Builder[F] =
    fromLogger(LoggerFactory[F].getLogger)

  def fromLogger[F[_]: Concurrent: Clock](logger: StructuredLogger[F]): Builder[F] =
    new Builder[F](
      logger,
      Defaults.willLog[F],
      Defaults.routeClassifier(_),
      Defaults.reqHeaders,
      Defaults.requestIncludeUrl(_),
      Defaults.requestObserveBody,
      Defaults.requestBodyEncoder,
      Defaults.requestBodyMaxSize,
      Defaults.respHeaders,
      Defaults.responseObserveBody,
      Defaults.responseBodyEncoder,
      Defaults.responseBodyMaxSize,
      Defaults.removedContextKeys,
      Defaults.additionalContext(_,_),
      Defaults.logLevel(_, _),
      Defaults.logMessage(_,_,_)
    )

  final class Builder[F[_]: Concurrent: Clock] private[ServerMiddleware](
    logger: StructuredLogger[F],
    willLog: Request[Pure] => F[Boolean],

    routeClassifier: Request[Pure] => Option[String],



    reqHeaders: Set[CIString],

    requestIncludeUrl: Request[Pure] => Boolean,
    requestObserveBody: Boolean,
    requestBodyEncoder: Request[Pure] => Option[String],
    requestBodyMaxSize: Long,

    respHeaders: Set[CIString],
    responseObserveBody: Boolean,
    responseBodyEncoder: Response[Pure] => Option[String],
    responseBodyMaxSize: Long,

    removedContextKeys: (Request[Pure], Outcome[Option, Throwable, Response[Pure]]) => Set[String],

    additionalContext: (Request[Pure], Outcome[Option, Throwable, Response[Pure]]) => Map[String, String],
    logLevel: (Request[Pure], Outcome[Option, Throwable, Response[Pure]]) => Option[LogLevel],
    logMessage: (Request[Pure], Outcome[Option, Throwable, Response[Pure]], FiniteDuration) => String,
  ){ self =>

    private def copy(
      logger: StructuredLogger[F] = self.logger,
      willLog: Request[Pure] => F[Boolean] = self.willLog,
      routeClassifier: Request[Pure] => Option[String] = self.routeClassifier,
      reqHeaders: Set[CIString] = self.reqHeaders,
      requestIncludeUrl: Request[Pure] => Boolean = self.requestIncludeUrl,
      requestObserveBody: Boolean = self.requestObserveBody,
      requestBodyEncoder: Request[Pure] => Option[String] = self.requestBodyEncoder,
      requestBodyMaxSize: Long = self.requestBodyMaxSize,
      respHeaders: Set[CIString] = self.respHeaders,
      responseObserveBody: Boolean = self.responseObserveBody,
      responseBodyEncoder: Response[Pure] => Option[String] = self.responseBodyEncoder,
      responseBodyMaxSize: Long = self.responseBodyMaxSize,
      removedContextKeys: (Request[Pure], Outcome[Option, Throwable, Response[Pure]]) => Set[String] = self.removedContextKeys,
      additionalContext: (Request[Pure], Outcome[Option, Throwable, Response[Pure]]) => Map[String, String] = self.additionalContext,
      logLevel: (Request[Pure], Outcome[Option, Throwable, Response[Pure]]) => Option[LogLevel] = self.logLevel,
      logMessage: (Request[Pure], Outcome[Option, Throwable, Response[Pure]], FiniteDuration) => String = self.logMessage,
    ) = new Builder[F](
      logger,
      willLog,
      routeClassifier,
      reqHeaders,
      requestIncludeUrl,
      requestObserveBody,
      requestBodyEncoder,
      requestBodyMaxSize,
      respHeaders,
      responseObserveBody,
      responseBodyEncoder,
      responseBodyMaxSize,
      removedContextKeys,
      additionalContext,
      logLevel,
      logMessage
    )


    def withRemovedContextKeys(removedContextKeys: (Request[Pure], Outcome[Option, Throwable, Response[Pure]]) =>Set[String]) =
      copy(removedContextKeys = removedContextKeys)
    def withStaticRemovedContextKeys(removedContextKeys: Set[String]) =
      withRemovedContextKeys((_,_) => removedContextKeys)

    def withWillLog(willLog: Request[Pure] => F[Boolean]) =
      copy(willLog = willLog)

    def withRouteClassifier(routeClassifier: Request[Pure] => Option[String]) =
      copy(routeClassifier = routeClassifier)
    def withIncludeUrl(includeUrl: Request[Pure] => Boolean) =
      copy(requestIncludeUrl = includeUrl)

    def withObserveRequestBody(boolean: Boolean) =
      copy(requestObserveBody = boolean)
    def withObserveResponseBody(boolean: Boolean) =
      copy(responseObserveBody = boolean)

    def withRequestBodyEncoder(encoder: Request[Pure] => Option[String]) =
      copy(requestBodyEncoder = encoder)
    def withResponseBodyEncoder(encoder: Response[Pure] => Option[String]) = 
      copy(responseBodyEncoder = encoder)

    def withRequestBodyMaxSize(l: Long) =
      copy(requestBodyMaxSize = l)
    def withResponseBodyMaxSize(l: Long) =
      copy(responseBodyMaxSize = l)

    def withAdditionalContext(additionalContext: (Request[Pure], Outcome[Option, Throwable, Response[Pure]]) => Map[String, String]) =
      copy(additionalContext = additionalContext)

    def withLogLevel(logLevel: (Request[Pure], Outcome[Option, Throwable, Response[Pure]]) => Option[LogLevel]) =
      copy(logLevel = logLevel)
    def withStaticLogLevel(logLevel: Option[LogLevel]) =
      withLogLevel((_,_) => logLevel)

    def withLogMessage(logMessage: (Request[Pure], Outcome[Option, Throwable, Response[Pure]], FiniteDuration) => String) =
      copy(logMessage = logMessage)

    def withAllowedRequestHeaders(reqHeaders: Set[CIString]) =
      copy(reqHeaders = reqHeaders)
    def withAllowedResponseHeaders(respHeaders: Set[CIString]) =
      copy(respHeaders = respHeaders)

    def httpApp(app: HttpApp[F]): HttpApp[F] =
      if (requestObserveBody || responseObserveBody) httpAppWithBody[F](logger, willLog, routeClassifier, reqHeaders, requestIncludeUrl, requestObserveBody, requestBodyEncoder, requestBodyMaxSize, respHeaders, responseObserveBody, responseBodyEncoder, responseBodyMaxSize, removedContextKeys, additionalContext, logLevel, logMessage)(app)
      else httpAppNoBody[F](logger, willLog, routeClassifier, reqHeaders, requestIncludeUrl, respHeaders, removedContextKeys, additionalContext, logLevel, logMessage)(app)
    def httpRoutes(routes: HttpRoutes[F]): HttpRoutes[F] =
      if (requestObserveBody || responseObserveBody) httpRoutesWithBody[F](logger, willLog, routeClassifier, reqHeaders, requestIncludeUrl, requestObserveBody, requestBodyEncoder, requestBodyMaxSize, respHeaders,  responseObserveBody, responseBodyEncoder, responseBodyMaxSize, removedContextKeys, additionalContext, logLevel, logMessage)(routes)
      else httpRoutesNoBody(logger, willLog, routeClassifier, reqHeaders, requestIncludeUrl, respHeaders, removedContextKeys, additionalContext, logLevel, logMessage)(routes)

  }

  private def httpAppWithBody[F[_]: Concurrent: Clock](
    logger: StructuredLogger[F],
    willLog: Request[Pure] => F[Boolean],

    routeClassifier: Request[Pure] => Option[String],

    reqHeaders: Set[CIString],
    requestIncludeUrl: Request[Pure] => Boolean,
    requestObserveBody: Boolean,
    requestBodyEncoder: Request[Pure] => Option[String],
    requestBodyMaxSize: Long,

    respHeaders: Set[CIString],
    responseObserveBody: Boolean,
    responseBodyEncoder: Response[Pure] => Option[String],
    responseBodyMaxSize: Long,


    removedContextKeysF: (Request[Pure], Outcome[Option, Throwable, Response[Pure]]) => Set[String],
    additionalContext: (Request[Pure], Outcome[Option, Throwable, Response[Pure]]) => Map[String, String],
    logLevel: (Request[Pure], Outcome[Option, Throwable, Response[Pure]]) => Option[LogLevel],
    logMessage: (Request[Pure], Outcome[Option, Throwable, Response[Pure]], FiniteDuration) => String,
  )(routes: HttpApp[F]): HttpApp[F] = Kleisli{(req: Request[F]) =>
    val pureReq: Request[Pure] = pureRequest(req)
    willLog(pureReq).flatMap{ enabled =>
      if (!enabled) routes.run(req)
      else {
        Clock[F].realTime.flatMap{ start =>
          Concurrent[F].uncancelable(poll =>
            poll{
              for {
                reqBody <- Concurrent[F].ref(Option.empty[fs2.Chunk[Byte]])
                reqPipe = {(s: fs2.Stream[F, Byte]) => s.chunks.evalMap(chunk => reqBody.update{
                  case Some(current) => (current ++ chunk).some
                  case None => chunk.some
                }).drain}
                newReq = {
                  if (requestObserveBody && req.contentLength.exists(l => l <= requestBodyMaxSize)){
                    req.withBodyStream(req.body.observe(reqPipe))
                  } else req
                }
                respBody <- Concurrent[F].ref(Option.empty[fs2.Chunk[Byte]])
                resp <- routes.run{
                  newReq
                }
                headersEnd <- Clock[F].realTime
                headersDuration = HttpStructuredContext.Common.headersDuration(headersEnd.minus(start))
              } yield {
                  resp.withBodyStream(
                    resp.body.observe((s: fs2.Stream[F, Byte]) =>
                      if (responseObserveBody && resp.contentLength.exists(l => l <= responseBodyMaxSize)) {
                        s.chunks.evalMap(chunk => respBody.update{
                          case Some(current) => (current ++ chunk).some
                          case None => chunk.some
                        }).drain
                      } else s.drain
                    )
                      .onFinalizeWeak{
                        val pureResp = pureResponse(resp)
                        for {
                          bodyEnd <- Clock[F].realTime
                          reqBodyFinal <- reqBody.get
                          bodyPureReq = reqBodyFinal.fold(pureReq)(chunk => pureReq.withBodyStream(Stream.chunk(chunk)))
                          requestBodyCtx = (
                            reqBodyFinal *>
                            requestBodyEncoder(bodyPureReq)
                            .map(body => Map("http.request.body" -> body))
                          ).getOrElse(Map.empty)
                          reqContext = request(bodyPureReq, reqHeaders, routeClassifier, requestIncludeUrl) +
                            HttpStructuredContext.Common.accessTime(start)
                          respBodyFinal <- respBody.get
                          respFinal = respBodyFinal.fold(pureResp)(body => pureResp.withBodyStream(Stream.chunk(body)))

                          bodyDuration = HttpStructuredContext.Common.bodyDuration(bodyEnd.minus(start))
                          
                          responseCtx = response(respFinal, respHeaders)
                          responseBodyCtx = (
                            respBodyFinal *> 
                            responseBodyEncoder(respFinal)
                            .map(body => Map("http.response.body" -> body))
                          )
                            .getOrElse(Map.empty)
                          outcome = Outcome.succeeded[Option, Throwable, Response[Pure]](respBodyFinal.fold(pureResp)(body => pureResp.withBodyStream(Stream.chunk(body))).some)
                          outcomeCtx = HttpStructuredContext.Common.outcome(outcome)
                          additionalCtx = additionalContext(bodyPureReq, outcome)
                          removedContextKeys = removedContextKeysF(pureReq, outcome)
                          finalCtx = reqContext ++ responseCtx + outcomeCtx + headersDuration + bodyDuration ++ requestBodyCtx ++ responseBodyCtx ++ additionalCtx
                          _ <- logLevelAware(logger, finalCtx, bodyPureReq, outcome, start, removedContextKeys, logLevel, logMessage)
                        } yield ()
                      }
                  )
              }
            }
              .guaranteeCase{
                case Outcome.Canceled() =>
                  Clock[F].realTime.flatMap{ end =>
                    val duration = HttpStructuredContext.Common.headersDuration(end.minus(start))
                    val outcome = Outcome.canceled[Option, Throwable, Response[Pure]]
                    val outcomeCtx = HttpStructuredContext.Common.outcome(outcome)
                    val reqContext = request(pureReq, reqHeaders, routeClassifier, requestIncludeUrl) +
                      HttpStructuredContext.Common.accessTime(start)
                    val additionalCtx = additionalContext(pureReq, outcome)
                    val removedContextKeys = removedContextKeysF(pureReq, outcome)
                    val finalCtx = reqContext + outcomeCtx + duration ++ additionalCtx
                    logLevelAware(logger, finalCtx, pureReq, outcome, start, removedContextKeys, logLevel, logMessage)
                  }
                case Outcome.Errored(e) =>
                  Clock[F].realTime.flatMap{ end =>
                    val duration = HttpStructuredContext.Common.headersDuration(end.minus(start))
                    val outcome = Outcome.errored[Option, Throwable, Response[Pure]](e)
                    val outcomeCtx = HttpStructuredContext.Common.outcome(outcome)
                    val reqContext = request(pureReq, reqHeaders, routeClassifier, requestIncludeUrl) +
                      HttpStructuredContext.Common.accessTime(start)
                    val additionalCtx = additionalContext(pureReq, outcome)
                    val removedContextKeys = removedContextKeysF(pureReq, outcome)
                    val finalCtx = reqContext + outcomeCtx + duration ++ additionalCtx
                    logLevelAware(logger, finalCtx, pureReq, outcome, start, removedContextKeys, logLevel, logMessage)
                  }
                case Outcome.Succeeded(_) =>  Applicative[F].unit
              }
          )
        }
      }
    }
  }

  private def httpRoutesWithBody[F[_]: Concurrent: Clock](
    logger: StructuredLogger[F],
    willLog: Request[Pure] => F[Boolean],

    routeClassifier: Request[Pure] => Option[String],

    reqHeaders: Set[CIString],

    requestIncludeUrl: Request[Pure] => Boolean,
    requestObserveBody: Boolean,
    requestBodyEncoder: Request[Pure] => Option[String],
    requestBodyMaxSize: Long,

    respHeaders: Set[CIString],

    responseObserveBody: Boolean,
    responseBodyEncoder: Response[Pure] => Option[String],
    responseBodyMaxSize: Long,

    removedContextKeysF: (Request[Pure], Outcome[Option, Throwable, Response[Pure]]) => Set[String],
    additionalContext: (Request[Pure], Outcome[Option, Throwable, Response[Pure]]) => Map[String, String],
    logLevel: (Request[Pure], Outcome[Option, Throwable, Response[Pure]]) => Option[LogLevel],
    logMessage: (Request[Pure], Outcome[Option, Throwable, Response[Pure]], FiniteDuration) => String,
  )(routes: HttpRoutes[F]): HttpRoutes[F] = Kleisli{(req: Request[F]) =>
    val pureReq: Request[Pure] = pureRequest(req)
    OptionT.liftF(willLog(pureReq)).flatMap{ enabled =>
      if (!enabled) routes.run(req)
      else {
        OptionT.liftF(Clock[F].realTime).flatMap{ start =>
          Concurrent[OptionT[F, *]].uncancelable(poll =>
            poll{ OptionT{
              for {
                reqBody <- Concurrent[F].ref(Option.empty[fs2.Chunk[Byte]])
                reqPipe = {(s: fs2.Stream[F, Byte]) => s.chunks.evalMap(chunk => reqBody.update{
                  case Some(current) => (current ++ chunk).some
                  case None => chunk.some
                }).drain}
                newReq = {
                  if (requestObserveBody && req.contentLength.exists(l => l <= requestBodyMaxSize)){
                    req.withBodyStream(req.body.observe(reqPipe))
                  } else req
                }
                respBody <- Concurrent[F].ref(Option.empty[fs2.Chunk[Byte]])
                respOpt <- routes.run{
                  newReq
                }.value
                headersEnd <- Clock[F].realTime
                headersDuration = HttpStructuredContext.Common.headersDuration(headersEnd.minus(start))
                out <- respOpt match {
                  case Some(resp) =>
                    resp.withBodyStream(
                      resp.body.observe((s: fs2.Stream[F, Byte]) =>
                        if (responseObserveBody && resp.contentLength.exists(l => l <= responseBodyMaxSize)) {
                          s.chunks.evalMap(chunk => respBody.update{
                            case Some(current) => (current ++ chunk).some
                            case None => chunk.some
                          }).drain
                        } else s.drain
                      )
                        .onFinalizeWeak{
                          val pureResp = pureResponse(resp)
                          for {
                            bodyEnd <- Clock[F].realTime
                            reqBodyFinal <- reqBody.get
                            newPureReq = reqBodyFinal.fold(pureReq)(chunk => pureReq.withBodyStream(Stream.chunk(chunk)))
                            reqContext = request(newPureReq, reqHeaders, routeClassifier, requestIncludeUrl) +
                              HttpStructuredContext.Common.accessTime(start)
                            respBodyFinal <- respBody.get
                            respPure = respBodyFinal.fold(pureResp)(body => pureResp.withBodyStream(Stream.chunk(body)))
                            bodyDuration = HttpStructuredContext.Common.bodyDuration(bodyEnd.minus(start))
                            requestBodyCtx = (
                              reqBodyFinal *>
                              requestBodyEncoder(newPureReq)
                            ).map(body => Map("http.request.body" -> body)).getOrElse(Map.empty)
                            responseCtx = response(respBodyFinal.fold(pureResp)(body => pureResp.withBodyStream(Stream.chunk(body))), respHeaders)
                            responseBodyCtx = (
                              respBodyFinal *>
                              responseBodyEncoder(respPure)
                            ).map(body => Map("http.response.body" -> body)).getOrElse(Map.empty)
                            outcome = Outcome.succeeded[Option, Throwable, Response[Pure]](respPure.some)
                            outcomeCtx = HttpStructuredContext.Common.outcome(outcome)
                            additionalCtx = additionalContext(newPureReq, outcome)
                            removedContextKeys = removedContextKeysF(newPureReq, outcome)
                            finalCtx = reqContext ++ responseCtx + outcomeCtx + headersDuration + bodyDuration ++ requestBodyCtx ++ responseBodyCtx ++ additionalCtx
                            _ <- logLevelAware(logger, finalCtx, newPureReq, outcome, start, removedContextKeys, logLevel, logMessage)
                          } yield ()
                        }
                    ).some.pure[F]
                  case None =>
                    val action = for {
                      bodyEnd <- Clock[F].realTime
                      reqBodyFinal <- reqBody.get
                      newPureReq = reqBodyFinal.fold(pureReq)(chunk => pureReq.withBodyStream(Stream.chunk(chunk)))
                      reqContext = request(newPureReq, reqHeaders, routeClassifier, requestIncludeUrl) +
                        HttpStructuredContext.Common.accessTime(start)
                      bodyDuration = HttpStructuredContext.Common.bodyDuration(bodyEnd.minus(start))
                      requestBodyCtx = (reqBodyFinal *> requestBodyEncoder(newPureReq)).map(body => Map("http.request.body" -> body)).getOrElse(Map.empty)
                      outcome = Outcome.succeeded[Option, Throwable, Response[Pure]](None)
                      outcomeCtx = HttpStructuredContext.Common.outcome(outcome)
                      additionalCtx = additionalContext(pureReq, outcome)
                      removedContextKeys = removedContextKeysF(pureReq, outcome)
                      finalCtx = reqContext ++ additionalCtx + outcomeCtx + headersDuration + bodyDuration ++ requestBodyCtx
                      _ <- logLevelAware(logger, finalCtx, newPureReq, outcome, start, removedContextKeys, logLevel, logMessage)
                    } yield ()
                    action.as(Option.empty[Response[F]])
                }
              } yield out
            }}
              .guaranteeCase{
                case Outcome.Canceled() =>
                  OptionT.liftF(Clock[F].realTime.flatMap{ end =>
                    val duration = HttpStructuredContext.Common.headersDuration(end.minus(start))
                    val outcome = Outcome.canceled[Option, Throwable, Response[Pure]]
                    val outcomeCtx = HttpStructuredContext.Common.outcome(outcome)
                    val reqContext = request(pureReq, reqHeaders, routeClassifier, requestIncludeUrl) +
                      HttpStructuredContext.Common.accessTime(start)
                    val additionalCtx = additionalContext(pureReq, outcome)
                    val removedContextKeys = removedContextKeysF(pureReq, outcome)
                    val finalCtx = reqContext + outcomeCtx + duration ++ additionalCtx
                    logLevelAware(logger, finalCtx, pureReq, outcome, start, removedContextKeys, logLevel, logMessage)
                  })
                case Outcome.Errored(e) =>
                  OptionT.liftF(Clock[F].realTime.flatMap{ end =>
                    val duration = HttpStructuredContext.Common.headersDuration(end.minus(start))
                    val outcome = Outcome.errored[Option, Throwable, Response[Pure]](e)
                    val outcomeCtx = HttpStructuredContext.Common.outcome(outcome)
                    val reqContext = request(pureReq, reqHeaders, routeClassifier, requestIncludeUrl) +
                      HttpStructuredContext.Common.accessTime(start)
                    val additionalCtx = additionalContext(pureReq, outcome)
                    val removedContextKeys = removedContextKeysF(pureReq, outcome)
                    val finalCtx = reqContext + outcomeCtx + duration ++ additionalCtx
                    logLevelAware(logger, finalCtx, pureReq, outcome, start, removedContextKeys, logLevel, logMessage)
                  })
                case Outcome.Succeeded(fa) => OptionT.liftF(Applicative[F].unit)
              }
          )
        }
      }
    }
  }


  private def httpAppNoBody[F[_]: Concurrent: Clock](
    logger: StructuredLogger[F],
    willLog: Request[Pure] => F[Boolean],

    routeClassifier: Request[Pure] => Option[String],

    reqHeaders: Set[CIString],
    requestIncludeUrl: Request[Pure] => Boolean,

    respHeaders: Set[CIString],

    removedContextKeysF: (Request[Pure], Outcome[Option, Throwable, Response[Pure]]) => Set[String],
    additionalContext: (Request[Pure], Outcome[Option, Throwable, Response[Pure]]) => Map[String, String],
    logLevel: (Request[Pure], Outcome[Option, Throwable, Response[Pure]]) => Option[LogLevel],
    logMessage: (Request[Pure], Outcome[Option, Throwable, Response[Pure]], FiniteDuration) => String,
  )(routes: HttpApp[F]): HttpApp[F] = Kleisli{(req: Request[F]) =>
    val pureReq: Request[Pure] = pureRequest(req)
    willLog(pureReq).flatMap{ enabled =>
      if (!enabled) routes.run(req)
      else {
        Clock[F].realTime.flatMap{ start =>
          val reqContext = request(pureReq, reqHeaders, routeClassifier, requestIncludeUrl) +
            HttpStructuredContext.Common.accessTime(start)
          Concurrent[F].uncancelable(poll =>
            poll(routes.run(req))
              .guaranteeCase{
                case Outcome.Canceled() =>
                  Clock[F].realTime.flatMap{ end =>
                    val duration = HttpStructuredContext.Common.headersDuration(end.minus(start))
                    val outcome = Outcome.canceled[Option, Throwable, Response[Pure]]
                    val outcomeCtx = HttpStructuredContext.Common.outcome(outcome)
                    val additionalCtx = additionalContext(pureReq, outcome)
                    val removedContextKeys = removedContextKeysF(pureReq, outcome)
                    val finalCtx = reqContext + outcomeCtx + duration ++ additionalCtx
                    logLevelAware(logger, finalCtx, pureReq, outcome, start, removedContextKeys, logLevel, logMessage)
                  }
                case Outcome.Errored(e) =>
                  Clock[F].realTime.flatMap{ end =>
                    val duration = HttpStructuredContext.Common.headersDuration(end.minus(start))
                    val outcome = Outcome.errored[Option, Throwable, Response[Pure]](e)
                    val outcomeCtx = HttpStructuredContext.Common.outcome(outcome)
                    val additionalCtx = additionalContext(pureReq, outcome)
                    val removedContextKeys = removedContextKeysF(pureReq, outcome)
                    val finalCtx = reqContext + outcomeCtx + duration ++ additionalCtx
                    logLevelAware(logger, finalCtx, pureReq, outcome, start, removedContextKeys, logLevel, logMessage)
                  }
                case Outcome.Succeeded(fa) => fa.flatMap{
                  case resp =>
                    val pureResp = pureResponse(resp)
                    Clock[F].realTime.flatMap{ end =>
                      val duration = HttpStructuredContext.Common.headersDuration(end.minus(start))
                      val responseCtx = response(pureResp, respHeaders)
                      val outcome = Outcome.succeeded[Option, Throwable, Response[Pure]](pureResp.some)
                      val outcomeCtx = HttpStructuredContext.Common.outcome(outcome)
                      val additionalCtx = additionalContext(pureReq, outcome)
                      val removedContextKeys = removedContextKeysF(pureReq, outcome)
                      val finalCtx = reqContext ++ responseCtx + outcomeCtx + duration ++ additionalCtx
                      logLevelAware(logger, finalCtx, pureReq, outcome, start, removedContextKeys, logLevel, logMessage)
                    }
                }
              }
          )
        }
      }
    }
  }

  private def httpRoutesNoBody[F[_]: Concurrent: Clock](
    logger: StructuredLogger[F],
    willLog: Request[Pure] => F[Boolean],

    routeClassifier: Request[Pure] => Option[String],

    reqHeaders: Set[CIString],
    requestIncludeUrl: Request[Pure] => Boolean,

    respHeaders: Set[CIString],

    removedContextKeysF: (Request[Pure], Outcome[Option, Throwable, Response[Pure]]) => Set[String],
    additionalContext: (Request[Pure], Outcome[Option, Throwable, Response[Pure]]) => Map[String, String],
    logLevel: (Request[Pure], Outcome[Option, Throwable, Response[Pure]]) => Option[LogLevel],
    logMessage: (Request[Pure], Outcome[Option, Throwable, Response[Pure]], FiniteDuration) => String,
  )(routes: HttpRoutes[F]): HttpRoutes[F] = Kleisli{(req: Request[F]) =>
    val pureReq: Request[Pure] = pureRequest(req)
    OptionT.liftF(willLog(pureReq)).flatMap{ enabled =>
      if (!enabled) routes.run(req)
      else {
        OptionT.liftF(Clock[F].realTime).flatMap{ start =>
          val reqContext = request(pureReq, reqHeaders, routeClassifier, requestIncludeUrl) +
            HttpStructuredContext.Common.accessTime(start)
          Concurrent[OptionT[F, *]].uncancelable(poll =>
            poll(routes.run(req))
              .guaranteeCase{
                case Outcome.Canceled() =>


                  OptionT.liftF{
                    Clock[F].realTime.flatMap{ end =>
                      val duration = HttpStructuredContext.Common.headersDuration(end.minus(start))
                      val outcome = Outcome.canceled[Option, Throwable, Response[Pure]]
                      val outcomeCtx = HttpStructuredContext.Common.outcome(outcome)
                      val additionalCtx = additionalContext(pureReq, outcome)
                      val removedContextKeys = removedContextKeysF(pureReq, outcome)
                      val finalCtx = reqContext + outcomeCtx + duration ++ additionalCtx
                      logLevelAware(logger, finalCtx, pureReq, outcome, start, removedContextKeys, logLevel, logMessage)
                    }
                  }
                case Outcome.Errored(e) =>

                  OptionT.liftF{
                    Clock[F].realTime.flatMap{ end =>
                      val duration = HttpStructuredContext.Common.headersDuration(end.minus(start))
                      val outcome = Outcome.errored[Option, Throwable, Response[Pure]](e)
                      val outcomeCtx = HttpStructuredContext.Common.outcome(outcome)
                      val additionalCtx = additionalContext(pureReq, outcome)
                      val removedContextKeys = removedContextKeysF(pureReq, outcome)
                      val finalCtx = reqContext+ outcomeCtx + duration ++ additionalCtx
                      logLevelAware(logger, finalCtx, pureReq, outcome, start, removedContextKeys, logLevel, logMessage)
                    }
                  }
                case Outcome.Succeeded(fa) => OptionT.liftF(fa.value.flatMap{
                  case option =>
                    Clock[F].realTime.flatMap{ end =>
                      val duration = HttpStructuredContext.Common.headersDuration(end.minus(start))
                      val responseCtx = option.map(resp => response(pureResponse(resp), respHeaders)).getOrElse(Map.empty)
                      val outcome = Outcome.succeeded[Option, Throwable, Response[Pure]](option.map(pureResponse))
                      val outcomeCtx = HttpStructuredContext.Common.outcome(outcome)
                      val additionalCtx = additionalContext(pureReq, outcome)
                      val removedContextKeys = removedContextKeysF(pureReq, outcome)
                      val finalCtx = reqContext ++ responseCtx + outcomeCtx + duration ++ additionalCtx
                      logLevelAware(logger, finalCtx, pureReq, outcome, start, removedContextKeys, logLevel, logMessage)
                    }
                })
              }
          )
        }
      }
    }
  }

  private def request[F[_]](request: Request[Pure], headers: Set[CIString], routeClassifier: Request[Pure] => Option[String], includeUrl: Request[Pure] => Boolean): Map[String, String] = {
    val builder = Map.newBuilder[String, String]
    builder += HttpStructuredContext.Common.logKind("server")
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

    builder.result()
  }

  private def response[F[_]](response: Response[Pure], headers: Set[CIString]): Map[String, String] = {
    val builder = Map.newBuilder[String, String]

    builder += HttpStructuredContext.Common.status(response.status)
    response.contentLength.foreach(l => 
      builder += HttpStructuredContext.Common.responseContentLength(l)
    )
    builder ++= 
      HttpStructuredContext.Headers.response(response.headers, headers)

    builder.result()
  }




}