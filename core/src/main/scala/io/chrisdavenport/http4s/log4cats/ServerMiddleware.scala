package io.chrisdavenport.http4s.log4cats


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
import scala.collection.immutable.MapBuilderImpl
import org.http4s.client.RequestKey
import cats.effect.kernel.Outcome.Canceled
import cats.effect.kernel.Outcome.Errored
import org.typelevel.vault.Key
import scala.concurrent.duration.FiniteDuration
import org.typelevel.log4cats.LoggerFactory


object ServerMiddleware {
  object Keys {
    val RequestContext = Key.newKey[SyncIO, Map[String, String]].unsafeRunSync()
  }
  object Defaults {
    def willLog[F[_]: Applicative](prelude: RequestPrelude): F[Boolean] = true.pure[F]
    def routeClassifier(prelude: RequestPrelude): Option[String] = None
    def reqHeaders = HttpStructuredContext.Headers.defaultHeadersAllowed
    def requestAdditionalContext(prelude: RequestPrelude) = Map.empty[String, String]
    def requestIncludeUrl(prelude: RequestPrelude) = true
    val requestLogBody = false
    val requestBodyMaxSize = 65535

    def respHeaders = HttpStructuredContext.Headers.defaultHeadersAllowed
    def responseAdditionalContext(prelude: ResponsePrelude) = Map.empty[String, String]
    val responseLogBody = false
    val responseBodyMaxSize = 65535
    def logLevel(prelude: RequestPrelude, outcome: Outcome[Option, Throwable, ResponsePrelude]): Option[LogLevel] = LogLevel.Info.some
    def logMessage(prelude: RequestPrelude, outcome: Outcome[Option, Throwable, ResponsePrelude], now: FiniteDuration): String = s"Http Server - ${prelude.method}"



  }

  def fromLoggerFactory[F[_]: Concurrent: Clock: LoggerFactory]: ServerMiddlewareBuilder[F] =
    fromLogger(LoggerFactory[F].getLogger)

  def fromLogger[F[_]: Concurrent: Clock](logger: SelfAwareStructuredLogger[F]): ServerMiddlewareBuilder[F] =
    new ServerMiddlewareBuilder[F](
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
      Defaults.logLevel(_, _),
      Defaults.logMessage(_,_,_)
    )

  final class ServerMiddlewareBuilder[F[_]: Concurrent: Clock] private[ServerMiddleware](
    logger: SelfAwareStructuredLogger[F],
    willLog: RequestPrelude => F[Boolean],

    routeClassifier: RequestPrelude => Option[String],


    reqHeaders: Set[CIString],
    requestAdditionalContext: RequestPrelude => Map[String, String],
    requestIncludeUrl: RequestPrelude => Boolean,
    requestLogBody: Boolean,
    requestBodyMaxSize: Long,

    respHeaders: Set[CIString],
    responseAdditionalContext: ResponsePrelude => Map[String, String],
    responseLogBody: Boolean,
    responseBodyMaxSize: Long,

    logLevel: (RequestPrelude, Outcome[Option, Throwable, ResponsePrelude]) => Option[LogLevel],
    logMessage: (RequestPrelude, Outcome[Option, Throwable, ResponsePrelude], FiniteDuration) => String,
  ){ self =>

    private def copy(
      logger: SelfAwareStructuredLogger[F] = self.logger,
      willLog: RequestPrelude => F[Boolean] = self.willLog,
      routeClassifier: RequestPrelude => Option[String] = self.routeClassifier,
      reqHeaders: Set[CIString] = self.reqHeaders,
      requestAdditionalContext: RequestPrelude => Map[String, String] = self.requestAdditionalContext,
      requestIncludeUrl: RequestPrelude => Boolean = self.requestIncludeUrl,
      requestLogBody: Boolean = self.requestLogBody,
      requestBodyMaxSize: Long = self.requestBodyMaxSize,
      respHeaders: Set[CIString] = self.respHeaders,
      responseAdditionalContext: ResponsePrelude => Map[String, String] = self.responseAdditionalContext,
      responseLogBody: Boolean = self.responseLogBody,
      responseBodyMaxSize: Long = self.responseBodyMaxSize,
      logLevel: (RequestPrelude, Outcome[Option, Throwable, ResponsePrelude]) => Option[LogLevel] = self.logLevel,
      logMessage: (RequestPrelude, Outcome[Option, Throwable, ResponsePrelude], FiniteDuration) => String = self.logMessage,
    ) = new ServerMiddlewareBuilder[F](
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
      logLevel,
      logMessage
    )

    def withWillLog(willLog: RequestPrelude => F[Boolean]) =
      copy(willLog = willLog)

    def withRouteClassifier(routeClassifier: RequestPrelude => Option[String]) =
      copy(routeClassifier = routeClassifier)
    def withIncludeUrl(includeUrl: RequestPrelude => Boolean) =
      copy(requestIncludeUrl = includeUrl)
    def withAdditionalRequestContext(requestAdditionalContext: RequestPrelude => Map[String, String]) =
      copy(requestAdditionalContext = requestAdditionalContext)
    def withAdditionalResponseContext(responseAdditionalContext: ResponsePrelude => Map[String, String]) =
      copy(responseAdditionalContext = responseAdditionalContext)

    def withLogRequestBody(boolean: Boolean) =
      copy(requestLogBody = boolean)
    def withLogResponseBody(boolean: Boolean) =
      copy(responseLogBody = boolean)

    def withRequestBodyMaxSize(l: Long) =
      copy(requestBodyMaxSize = l)
    def withResponseBodyMaxSize(l: Long) =
      copy(responseBodyMaxSize = l)

    def withLogLevel(logLevel: (RequestPrelude, Outcome[Option, Throwable, ResponsePrelude]) => Option[LogLevel]) =
      copy(logLevel = logLevel)
    def withLogMessage(logMessage: (RequestPrelude, Outcome[Option, Throwable, ResponsePrelude], FiniteDuration) => String) =
      copy(logMessage = logMessage)

    def withAllowedRequestHeaders(reqHeaders: Set[CIString]) =
      copy(reqHeaders = reqHeaders)
    def withAllowedResponseHeaders(respHeaders: Set[CIString]) =
      copy(respHeaders = respHeaders)

    def httpApp(app: HttpApp[F]): HttpApp[F] =
      if (requestLogBody || responseLogBody) httpAppWithBody[F](logger, willLog, routeClassifier, reqHeaders, requestAdditionalContext, requestIncludeUrl, requestLogBody, requestBodyMaxSize, respHeaders, responseAdditionalContext, responseLogBody, responseBodyMaxSize, logLevel, logMessage)(app)
      else httpAppNoBody[F](logger, willLog, routeClassifier, reqHeaders, requestAdditionalContext, requestIncludeUrl, respHeaders, responseAdditionalContext, logLevel, logMessage)(app)
    def httpRoutes(routes: HttpRoutes[F]): HttpRoutes[F] =
      if (requestLogBody || responseLogBody) httpRoutesWithBody[F](logger, willLog, routeClassifier, reqHeaders, requestAdditionalContext, requestIncludeUrl, requestLogBody, requestBodyMaxSize, respHeaders, responseAdditionalContext, responseLogBody, responseBodyMaxSize, logLevel, logMessage)(routes)
      else httpRoutesNoBody(logger, willLog, routeClassifier, reqHeaders, requestAdditionalContext, requestIncludeUrl, respHeaders, responseAdditionalContext, logLevel, logMessage)(routes)

  }

  private def httpAppWithBody[F[_]: Concurrent: Clock](
    logger: SelfAwareStructuredLogger[F],
    willLog: RequestPrelude => F[Boolean],

    routeClassifier: RequestPrelude => Option[String],

    reqHeaders: Set[CIString],
    requestAdditionalContext: RequestPrelude => Map[String, String],
    requestIncludeUrl: RequestPrelude => Boolean,
    requestLogBody: Boolean,
    requestBodyMaxSize: Long,

    respHeaders: Set[CIString],
    responseAdditionalContext: ResponsePrelude => Map[String, String],
    responseLogBody: Boolean,
    responseBodyMaxSize: Long,

    logLevel: (RequestPrelude, Outcome[Option, Throwable, ResponsePrelude]) => Option[LogLevel],
    logMessage: (RequestPrelude, Outcome[Option, Throwable, ResponsePrelude], FiniteDuration) => String,
  )(routes: HttpApp[F]): HttpApp[F] = Kleisli{(req: Request[F]) =>
    willLog(req.requestPrelude).flatMap{ enabled =>
      if (!enabled) routes.run(req)
      else {
        Clock[F].realTime.flatMap{ start =>
          val reqContext = request(req, reqHeaders, routeClassifier, requestIncludeUrl, requestAdditionalContext)
          Concurrent[F].uncancelable(poll =>
            poll{
              for {
                reqBody <- Concurrent[F].ref(Option.empty[fs2.Chunk[Byte]])
                reqPipe = {(s: fs2.Stream[F, Byte]) => s.chunks.evalMap(chunk => reqBody.update{
                  case Some(current) => (current ++ chunk).some
                  case None => chunk.some
                }).drain}
                newReq = {
                  if (requestLogBody && req.contentLength.exists(l => l <= requestBodyMaxSize)){
                    req.withBodyStream(req.body.observe(reqPipe))
                  } else req
                }
                respBody <- Concurrent[F].ref(Option.empty[fs2.Chunk[Byte]])
                resp <- routes.run{
                  newReq.withAttribute(Keys.RequestContext, reqContext)
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
                        for {
                          end <- Clock[F].realTime
                          reqBodyFinal <- reqBody.get
                          reqBodyS <- reqBodyFinal.traverse(chunk => logBody(req.withBodyStream(fs2.Stream.chunk(chunk))))
                          respBodyFinal <- respBody.get
                          respBodyS <- respBodyFinal.traverse(chunk => logBody(resp.withBodyStream(fs2.Stream.chunk(chunk))))
                          duration = "http.duration_ms" -> end.minus(start).toMillis.toString()
                          requestBodyCtx = reqBodyS.map(body => Map("http.request.body" -> body)).getOrElse(Map.empty)
                          responseCtx = response(resp, respHeaders, responseAdditionalContext)
                          responseBodyCtx = respBodyS.map(body => Map("http.response.body" -> body)).getOrElse(Map.empty)
                          outcome = Outcome.succeeded[Option, Throwable, ResponsePrelude](resp.responsePrelude.some)
                          outcomeCtx = outcomeContext(outcome)
                          finalCtx = reqContext ++ responseCtx + outcomeCtx + duration ++ requestBodyCtx ++ responseBodyCtx
                          _ <- logLevelAware(logger, finalCtx, req.requestPrelude, outcome, end, logLevel, logMessage)
                        } yield ()
                      }
                  )
              }
            }
              .guaranteeCase{
                case Outcome.Canceled() =>
                  Clock[F].realTime.flatMap{ end =>
                    val duration = "http.duration_ms" -> end.minus(start).toMillis.toString()
                    val outcome = Outcome.canceled[Option, Throwable, ResponsePrelude]
                    val outcomeCtx = outcomeContext(outcome)
                    val finalCtx = reqContext + outcomeCtx + duration
                    logLevelAware(logger, finalCtx, req.requestPrelude, outcome, end, logLevel, logMessage)
                  }
                case Outcome.Errored(e) =>
                  Clock[F].realTime.flatMap{ end =>
                    val duration = "http.duration_ms" -> end.minus(start).toMillis.toString()
                    val outcome = Outcome.errored[Option, Throwable, ResponsePrelude](e)
                    val outcomeCtx = outcomeContext(outcome)
                    val finalCtx = reqContext + outcomeCtx + duration
                    logLevelAware(logger, finalCtx, req.requestPrelude, outcome, end, logLevel, logMessage)
                  }
                case Outcome.Succeeded(_) =>  Applicative[F].unit
              }
          )
        }
      }
    }
  }

  private def httpRoutesWithBody[F[_]: Concurrent: Clock](
    logger: SelfAwareStructuredLogger[F],
    willLog: RequestPrelude => F[Boolean],

    routeClassifier: RequestPrelude => Option[String],

    reqHeaders: Set[CIString],
    requestAdditionalContext: RequestPrelude => Map[String, String],
    requestIncludeUrl: RequestPrelude => Boolean,
    requestLogBody: Boolean,
    requestBodyMaxSize: Long,

    respHeaders: Set[CIString],
    responseAdditionalContext: ResponsePrelude => Map[String, String],
    responseLogBody: Boolean,
    responseBodyMaxSize: Long,

    logLevel: (RequestPrelude, Outcome[Option, Throwable, ResponsePrelude]) => Option[LogLevel],
    logMessage: (RequestPrelude, Outcome[Option, Throwable, ResponsePrelude], FiniteDuration) => String,
  )(routes: HttpRoutes[F]): HttpRoutes[F] = Kleisli{(req: Request[F]) =>
    OptionT.liftF(willLog(req.requestPrelude)).flatMap{ enabled =>
      if (!enabled) routes.run(req)
      else {
        OptionT.liftF(Clock[F].realTime).flatMap{ start =>
          val reqContext = request(req, reqHeaders, routeClassifier, requestIncludeUrl, requestAdditionalContext)
          Concurrent[OptionT[F, *]].uncancelable(poll =>
            poll{
              for {
                reqBody <- OptionT.liftF(Concurrent[F].ref(Option.empty[fs2.Chunk[Byte]]))
                reqPipe = {(s: fs2.Stream[F, Byte]) => s.chunks.evalMap(chunk => reqBody.update{
                  case Some(current) => (current ++ chunk).some
                  case None => chunk.some
                }).drain}
                newReq = {
                  if (requestLogBody && req.contentLength.exists(l => l <= requestBodyMaxSize)){
                    req.withBodyStream(req.body.observe(reqPipe))
                  } else req
                }
                respBody <- OptionT.liftF(Concurrent[F].ref(Option.empty[fs2.Chunk[Byte]]))
                resp <- routes.run{
                  newReq.withAttribute(Keys.RequestContext, reqContext)
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
                        for {
                          end <- Clock[F].realTime
                          reqBodyFinal <- reqBody.get
                          reqBodyS <- reqBodyFinal.traverse(chunk => logBody(req.withBodyStream(fs2.Stream.chunk(chunk))))
                          respBodyFinal <- respBody.get
                          respBodyS <- respBodyFinal.traverse(chunk => logBody(resp.withBodyStream(fs2.Stream.chunk(chunk))))
                          duration = "http.duration_ms" -> end.minus(start).toMillis.toString()
                          requestBodyCtx = reqBodyS.map(body => Map("http.request.body" -> body)).getOrElse(Map.empty)
                          responseCtx = response(resp, respHeaders, responseAdditionalContext)
                          responseBodyCtx = respBodyS.map(body => Map("http.response.body" -> body)).getOrElse(Map.empty)
                          outcome = Outcome.succeeded[Option, Throwable, ResponsePrelude](resp.responsePrelude.some)
                          outcomeCtx = outcomeContext(outcome)
                          finalCtx = reqContext ++ responseCtx + outcomeCtx + duration ++ requestBodyCtx ++ responseBodyCtx
                          _ <- logLevelAware(logger, finalCtx, req.requestPrelude, outcome, end, logLevel, logMessage)
                        } yield ()
                      }
                  )
              }
            }
              .guaranteeCase{
                case Outcome.Canceled() =>
                  OptionT.liftF(Clock[F].realTime.flatMap{ end =>
                    val duration = "http.duration_ms" -> end.minus(start).toMillis.toString()
                    val outcome = Outcome.canceled[Option, Throwable, ResponsePrelude]
                    val outcomeCtx = outcomeContext(outcome)
                    val finalCtx = reqContext + outcomeCtx + duration
                    logLevelAware(logger, finalCtx, req.requestPrelude, outcome, end, logLevel, logMessage)
                  })
                case Outcome.Errored(e) =>
                  OptionT.liftF(Clock[F].realTime.flatMap{ end =>
                    val duration = "http.duration_ms" -> end.minus(start).toMillis.toString()
                    val outcome = Outcome.errored[Option, Throwable, ResponsePrelude](e)
                    val outcomeCtx = outcomeContext(outcome)
                    val finalCtx = reqContext + outcomeCtx + duration
                    logLevelAware(logger, finalCtx, req.requestPrelude, outcome, end, logLevel, logMessage)
                  })
                case Outcome.Succeeded(fa) => OptionT.liftF(fa.value.flatMap{
                  case None =>
                    // TODO move this case into poll so we can log the request body still.
                    Clock[F].realTime.flatMap{ end =>
                      val duration = "http.duration_ms" -> end.minus(start).toMillis.toString()
                      val outcome = Outcome.succeeded[Option, Throwable, ResponsePrelude](Option.empty)
                      val outcomeCtx = outcomeContext(outcome)
                      val finalCtx = reqContext + outcomeCtx + duration
                      logLevelAware(logger, finalCtx, req.requestPrelude, outcome, end, logLevel, logMessage)
                    }
                  case Some(_) => Applicative[F].unit
                })
              }
          )
        }
      }
    }
  }

  private def logBody[F[_]: Concurrent](message: Message[F]): F[String] = {
    val isBinary = message.contentType.exists(_.mediaType.binary)
    val isJson = message.contentType.exists(mT =>
      mT.mediaType == MediaType.application.json || mT.mediaType.subType.endsWith("+json")
    )
    if (!isBinary || isJson) {
      message
        .bodyText(implicitly, message.charset.getOrElse(Charset.`UTF-8`))
        .compile
        .string
    }else message.body.compile.to(scodec.bits.ByteVector).map(_.toHex)

  }

  private def httpAppNoBody[F[_]: Concurrent: Clock](
    logger: SelfAwareStructuredLogger[F],
    willLog: RequestPrelude => F[Boolean],

    routeClassifier: RequestPrelude => Option[String],

    reqHeaders: Set[CIString],
    requestAdditionalContext: RequestPrelude => Map[String, String],
    requestIncludeUrl: RequestPrelude => Boolean,

    respHeaders: Set[CIString],
    responseAdditionalContext: ResponsePrelude => Map[String, String],

    logLevel: (RequestPrelude, Outcome[Option, Throwable, ResponsePrelude]) => Option[LogLevel],
    logMessage: (RequestPrelude, Outcome[Option, Throwable, ResponsePrelude], FiniteDuration) => String,
  )(routes: HttpApp[F]): HttpApp[F] = Kleisli{(req: Request[F]) =>
    willLog(req.requestPrelude).flatMap{ enabled =>
      if (!enabled) routes.run(req)
      else {
        Clock[F].realTime.flatMap{ start =>
          val reqContext = request(req, reqHeaders, routeClassifier, requestIncludeUrl, requestAdditionalContext)
          Concurrent[F].uncancelable(poll =>
            poll(routes.run(req.withAttribute(Keys.RequestContext, reqContext)))
              .guaranteeCase{
                case Outcome.Canceled() =>
                  Clock[F].realTime.flatMap{ end =>
                    val duration = "http.duration_ms" -> end.minus(start).toMillis.toString()
                    val outcome = Outcome.canceled[Option, Throwable, ResponsePrelude]
                    val outcomeCtx = outcomeContext(outcome)
                    val finalCtx = reqContext + outcomeCtx + duration
                    logLevelAware(logger, finalCtx, req.requestPrelude, outcome, end, logLevel, logMessage)
                  }
                case Outcome.Errored(e) =>
                  Clock[F].realTime.flatMap{ end =>
                    val duration = "http.duration_ms" -> end.minus(start).toMillis.toString()
                    val outcome = Outcome.errored[Option, Throwable, ResponsePrelude](e)
                    val outcomeCtx = outcomeContext(outcome)
                    val finalCtx = reqContext + outcomeCtx + duration
                    logLevelAware(logger, finalCtx, req.requestPrelude, outcome, end, logLevel, logMessage)
                  }
                case Outcome.Succeeded(fa) => fa.flatMap{
                  case resp =>
                    Clock[F].realTime.flatMap{ end =>
                      val duration = "http.duration_ms" -> end.minus(start).toMillis.toString()
                      val responseCtx = response(resp, respHeaders, responseAdditionalContext)
                      val outcome = Outcome.succeeded[Option, Throwable, ResponsePrelude](resp.responsePrelude.some)
                      val outcomeCtx = outcomeContext(outcome)
                      val finalCtx = reqContext ++ responseCtx + outcomeCtx + duration
                      logLevelAware(logger, finalCtx, req.requestPrelude, outcome, end, logLevel, logMessage)
                    }
                }
              }
          )
        }
      }
    }
  }

  private def httpRoutesNoBody[F[_]: Concurrent: Clock](
    logger: SelfAwareStructuredLogger[F],
    willLog: RequestPrelude => F[Boolean],

    routeClassifier: RequestPrelude => Option[String],

    reqHeaders: Set[CIString],
    requestAdditionalContext: RequestPrelude => Map[String, String],
    requestIncludeUrl: RequestPrelude => Boolean,

    respHeaders: Set[CIString],
    responseAdditionalContext: ResponsePrelude => Map[String, String],

    logLevel: (RequestPrelude, Outcome[Option, Throwable, ResponsePrelude]) => Option[LogLevel],
    logMessage: (RequestPrelude, Outcome[Option, Throwable, ResponsePrelude], FiniteDuration) => String,
  )(routes: HttpRoutes[F]): HttpRoutes[F] = Kleisli{(req: Request[F]) =>
    OptionT.liftF(willLog(req.requestPrelude)).flatMap{ enabled =>
      if (!enabled) routes.run(req)
      else {
        OptionT.liftF(Clock[F].realTime).flatMap{ start =>
          val reqContext = request(req, reqHeaders, routeClassifier, requestIncludeUrl, requestAdditionalContext)
          Concurrent[OptionT[F, *]].uncancelable(poll =>
            poll(routes.run(req.withAttribute(Keys.RequestContext, reqContext)))
              .guaranteeCase{
                case Outcome.Canceled() =>


                  OptionT.liftF{
                    Clock[F].realTime.flatMap{ end =>
                      val duration = "http.duration_ms" -> end.minus(start).toMillis.toString()
                      val outcome = Outcome.canceled[Option, Throwable, ResponsePrelude]
                      val outcomeCtx = outcomeContext(outcome)
                      val finalCtx = reqContext + outcomeCtx + duration
                      logLevelAware(logger, finalCtx, req.requestPrelude, outcome, end, logLevel, logMessage)
                    }
                  }
                case Outcome.Errored(e) =>

                  OptionT.liftF{
                    Clock[F].realTime.flatMap{ end =>
                      val duration = "http.duration_ms" -> end.minus(start).toMillis.toString()
                      val outcome = Outcome.errored[Option, Throwable, ResponsePrelude](e)
                      val outcomeCtx = outcomeContext(outcome)
                      val finalCtx = reqContext+ outcomeCtx + duration
                      logLevelAware(logger, finalCtx, req.requestPrelude, outcome, end, logLevel, logMessage)
                    }
                  }
                case Outcome.Succeeded(fa) => OptionT.liftF(fa.value.flatMap{
                  case option =>
                    Clock[F].realTime.flatMap{ end =>
                      val duration = "http.duration_ms" -> end.minus(start).toMillis.toString()
                      val responseCtx = option.map(response(_, respHeaders, responseAdditionalContext)).getOrElse(Map.empty)
                      val outcome = Outcome.succeeded[Option, Throwable, ResponsePrelude](option.map(_.responsePrelude))
                      val outcomeCtx = outcomeContext(outcome)
                      val finalCtx = reqContext ++ responseCtx + outcomeCtx + duration
                      logLevelAware(logger, finalCtx, req.requestPrelude, outcome, end, logLevel, logMessage)
                    }
                })
              }
          )
        }
      }
    }
  }

  private def outcomeContext(outcome: Outcome[Option, Throwable, ResponsePrelude]): (String, String) = {
    outcome match {
      case Outcome.Canceled() => "exit.case" -> "canceled"
      case Outcome.Errored(_) => "exit.case" -> "errored"
      case Outcome.Succeeded(_) => "exit.case" -> "succeeded"
    }
  }

  private def logLevelAware[F[_]: Applicative](
    logger: StructuredLogger[F],
    ctx: Map[String, String],
    prelude: RequestPrelude,
    outcome: Outcome[Option, Throwable, ResponsePrelude],
    now: FiniteDuration,
    logLevel: (RequestPrelude, Outcome[Option, Throwable, ResponsePrelude]) => Option[LogLevel],
    logMessage: (RequestPrelude, Outcome[Option, Throwable, ResponsePrelude], FiniteDuration) => String,
  ): F[Unit] = {
    (logLevel(prelude, outcome), outcome) match {
      case (None, _) => Applicative[F].unit
      case (Some(LogLevel.Trace), Outcome.Errored(e)) =>
        logger.trace(ctx, e)(logMessage(prelude, outcome, now))
      case (Some(LogLevel.Trace), _) =>
        logger.trace(ctx)(logMessage(prelude, outcome, now))
      case (Some(LogLevel.Debug), Outcome.Errored(e)) =>
        logger.debug(ctx, e)(logMessage(prelude, outcome, now))
      case (Some(LogLevel.Debug), _) =>
        logger.debug(ctx)(logMessage(prelude, outcome, now))
      case (Some(LogLevel.Info), Outcome.Errored(e)) =>
        logger.info(ctx, e)(logMessage(prelude, outcome, now))
      case (Some(LogLevel.Info), _) =>
        logger.info(ctx)(logMessage(prelude, outcome, now))
      case (Some(LogLevel.Warn), Outcome.Errored(e)) =>
        logger.warn(ctx, e)(logMessage(prelude, outcome, now))
      case (Some(LogLevel.Warn), _) =>
        logger.warn(ctx)(logMessage(prelude, outcome, now))
      case (Some(LogLevel.Error), Outcome.Errored(e)) =>
        logger.error(ctx, e)(logMessage(prelude, outcome, now))
      case (Some(LogLevel.Error), _) =>
        logger.error(ctx)(logMessage(prelude, outcome, now))
    }
  }

  private def request[F[_]](request: Request[F], headers: Set[CIString], routeClassifier: RequestPrelude => Option[String], includeUrl: RequestPrelude => Boolean, additionalRequestContext: RequestPrelude => Map[String, String]): Map[String, String] = {
    val builder = MapBuilderImpl[String, String]()
    val prelude = request.requestPrelude
    builder += HttpStructuredContext.Common.method(request.method)
    if (includeUrl(request.requestPrelude)) {
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
    routeClassifier(prelude).foreach(s =>
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

    builder ++= additionalRequestContext(prelude)


    builder.result()
  }

  def response[F[_]](response: Response[F], headers: Set[CIString], responseAdditionalContext: ResponsePrelude => Map[String, String]): Map[String, String] = {
    val builder = MapBuilderImpl[String, String]()

    builder += HttpStructuredContext.Common.status(response.status)
    response.contentLength.foreach(l => 
      builder += HttpStructuredContext.Common.responseContentLength(l)
    )
    builder ++= 
      HttpStructuredContext.Headers.response(response.headers, headers)

    builder ++= responseAdditionalContext(response.responsePrelude)

    builder.result()
  }
}