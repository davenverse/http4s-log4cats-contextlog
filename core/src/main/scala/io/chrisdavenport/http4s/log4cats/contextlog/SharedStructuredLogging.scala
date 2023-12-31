package io.chrisdavenport.http4s.log4cats.contextlog

import cats._
import cats.syntax.all._
import cats.effect._
import fs2.{Stream, Pure}
import cats.effect.Outcome
import org.typelevel.log4cats.StructuredLogger
import org.typelevel.log4cats.extras.LogLevel
import scala.concurrent.duration._
import org.http4s._


private[contextlog] object SharedStructuredLogging {
  private[contextlog] def pureRequest[F[_]](req: Request[F]): Request[Pure] = Request(req.method, req.uri, req.httpVersion, req.headers, Stream.empty, req.attributes)
  private[contextlog] def pureResponse[F[_]](resp: Response[F]): Response[Pure] = Response(resp.status, resp.httpVersion, resp.headers, Stream.empty, resp.attributes)

  private[contextlog] def logLevel(defaultLevel: Option[LogLevel])(prelude: Request[Pure], outcome: Outcome[Option, Throwable, Response[Pure]]): Option[LogLevel] = {
    val _ = prelude
    outcome match {
      case Outcome.Succeeded(Some(resp)) =>
        resp.status.responseClass match {
          case Status.Informational => defaultLevel
          case Status.Successful    => defaultLevel
          case Status.Redirection   => defaultLevel
          case Status.ClientError =>
            if (resp.status.code === 404) defaultLevel
            else LogLevel.Warn.some
          case Status.ServerError => LogLevel.Error.some

        }
      case Outcome.Succeeded(None) => defaultLevel
      case Outcome.Canceled()      => LogLevel.Warn.some
      case Outcome.Errored(_)      => LogLevel.Error.some
    }
  }


  private[contextlog] def logLevelAware[F[_]: Applicative](
    logger: StructuredLogger[F],
    ctx: Map[String, String],
    prelude: Request[Pure],
    outcome: Outcome[Option, Throwable, Response[Pure]],
    now: FiniteDuration,
    removedContextKeys: Set[String],
    logLevel: (Request[Pure], Outcome[Option, Throwable, Response[Pure]]) => Option[LogLevel],
    logMessage: (Request[Pure], Outcome[Option, Throwable, Response[Pure]], FiniteDuration) => String,
  ): F[Unit] = {
    (logLevel(prelude, outcome), outcome) match {
      case (None, _) => Applicative[F].unit
      case (Some(LogLevel.Trace), Outcome.Errored(e)) =>
        logger.trace(ctx -- removedContextKeys, e)(logMessage(prelude, outcome, now))
      case (Some(LogLevel.Trace), _) =>
        logger.trace(ctx -- removedContextKeys)(logMessage(prelude, outcome, now))
      case (Some(LogLevel.Debug), Outcome.Errored(e)) =>
        logger.debug(ctx -- removedContextKeys, e)(logMessage(prelude, outcome, now))
      case (Some(LogLevel.Debug), _) =>
        logger.debug(ctx -- removedContextKeys)(logMessage(prelude, outcome, now))
      case (Some(LogLevel.Info), Outcome.Errored(e)) =>
        logger.info(ctx -- removedContextKeys, e)(logMessage(prelude, outcome, now))
      case (Some(LogLevel.Info), _) =>
        logger.info(ctx -- removedContextKeys)(logMessage(prelude, outcome, now))
      case (Some(LogLevel.Warn), Outcome.Errored(e)) =>
        logger.warn(ctx -- removedContextKeys, e)(logMessage(prelude, outcome, now))
      case (Some(LogLevel.Warn), _) =>
        logger.warn(ctx -- removedContextKeys)(logMessage(prelude, outcome, now))
      case (Some(LogLevel.Error), Outcome.Errored(e)) =>
        logger.error(ctx -- removedContextKeys, e)(logMessage(prelude, outcome, now))
      case (Some(LogLevel.Error), _) =>
        logger.error(ctx -- removedContextKeys)(logMessage(prelude, outcome, now))
    }
  }

  private[contextlog] def logBody(message: Message[Pure]): String = {
    val isBinary = message.contentType.exists(_.mediaType.binary)
    val isJson = message.contentType.exists(mT =>
      mT.mediaType == MediaType.application.json || mT.mediaType.subType.endsWith("+json")
    )
    val binary: scodec.bits.ByteVector = message.body.compile.to(scodec.bits.ByteVector)
    if (!isBinary || isJson) {

      binary.decodeStringLenient(
        replaceMalformedInput= true,
        replaceUnmappableChars = true,
        replacement= "�"
      )(message.charset.getOrElse(Charset.`UTF-8`).nioCharset)
    }else binary.toHex
  }


}