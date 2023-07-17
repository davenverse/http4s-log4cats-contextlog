package io.chrisdavenport.http4s.log4cats.contextlog

import java.time._
import java.time.format.DateTimeFormatter
import cats.effect._
import org.http4s._
import fs2._
import scala.concurrent.duration.FiniteDuration
import org.typelevel.ci._

object CommonLog {
  private val LSB = "["
  private val RSB = "]"
  private val DASH = "-"
  private val SPACE = " "
  private val DQUOTE = "\""

  private val dateTimeFormat = DateTimeFormatter.ofPattern("dd/MMM/yyyy:HH:mm:ss Z")

  // TODO make into a builder.
  def logMessage(
    zone: ZoneId,
    combined: Boolean,
    prefixed: Boolean,
    isClient: Boolean,
    // These two protocols are not implemented, however these values are useful to convey identity
    // if the values contains spaces, these should be wrapped by something, perhaps double quotes,
    // to indicate for parsers.
    identF: (Request[Pure], Outcome[Option, Throwable, Response[Pure]]) => Option[String] = (_,_) => None,
    userF: (Request[Pure], Outcome[Option, Throwable, Response[Pure]]) => Option[String] = (_,_) => None,
  )(
    request: Request[Pure],
    outcome: Outcome[Option, Throwable, Response[Pure]],
    now: FiniteDuration
  ): String = {

    val dateString = Instant.ofEpochMilli(now.toMillis).atZone(zone).format(dateTimeFormat)

    val statusS = outcome match {
      case Outcome.Succeeded(Some(resp)) => resp.status.code.toString()
      case Outcome.Succeeded(None) => DASH
      case Outcome.Errored(e) => DASH
      case Outcome.Canceled() => DASH
    }
    val respLengthS = outcome match {
      case Outcome.Succeeded(Some(resp)) => resp.contentLength.fold(DASH)(l => l.toString())
      case Outcome.Succeeded(None) => DASH
      case Outcome.Errored(e) => DASH
      case Outcome.Canceled() => DASH
    }

    val identS = identF(request, outcome).getOrElse(DASH)
    val userS = userF(request, outcome).getOrElse(DASH)

    val sb = new StringBuilder()
    if (prefixed){
      if (isClient) sb.append("HttpClient ")
      else sb.append("HttpServer ")
    }
    if (isClient) sb.append(DASH)
    else sb.append(request.from.fold(DASH)(_.toString())) // Remote
    sb.append(SPACE)

    sb.append(identS)
    sb.append(SPACE)

    sb.append(userS)
    sb.append(SPACE)

    sb.append(LSB)
    sb.append(dateString)
    sb.append(RSB)
    sb.append(SPACE)

    sb.append(DQUOTE)

    sb.append(request.method.renderString)
    sb.append(SPACE)
    if (isClient) sb.append(request.uri.renderString)
    else sb.append(request.uri.toOriginForm.renderString)

    sb.append(SPACE)
    sb.append(request.httpVersion.renderString)


    sb.append(DQUOTE)
    sb.append(SPACE)


    sb.append(statusS)
    sb.append(SPACE)

    sb.append(respLengthS)

    if (combined){
      sb.append(SPACE)
      request.headers.get(ci"Referer") match {
        case Some(referer) =>
          val raw = referer.head
          sb.append(DQUOTE)
          sb.append(raw.value)
          sb.append(DQUOTE)
        case None =>
          sb.append(DASH)
      }
      sb.append(SPACE)
      request.headers.get(ci"User-Agent") match {
        case Some(agent) =>
          val raw = agent.head
          sb.append(DQUOTE)
          sb.append(raw.value)
          sb.append(DQUOTE)
        case None =>
          sb.append(DASH)
      }
    }

    sb.toString()
  }

}