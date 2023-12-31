package io.chrisdavenport.http4s.log4cats.contextlog


import org.http4s._
import org.typelevel.ci.CIString
import org.http4s.headers._
import cats.syntax.all._
import com.comcast.ip4s._
import scala.concurrent.duration.FiniteDuration
import cats.effect.Outcome


object HttpStructuredContext {

  object Common {
    def method(m: Method): (String, String) = ("http.request.method", m.name)
    def url(url: Uri): (String, String)= ("http.request.url", url.renderString)
    def target(url: Uri): (String, String) = ("http.request.target", url.copy(scheme = None, authority = None).renderString)
    def host(host: org.http4s.headers.Host): (String, String) = ("http.request.host", org.http4s.headers.Host.headerInstance.value(host))
    def scheme(scheme: Uri.Scheme): (String, String) = ("http.request.scheme", scheme.value)
    
    def status(status: Status): (String, String) = ("http.response.status_code", status.code.show)
    // Need to check both request and response in case negotiation happens
    def flavor(httpVersion: HttpVersion): (String, String) = ("http.flavor", httpVersion.major.toString() ++ "." ++ httpVersion.minor.toString())
    def userAgent(userAgent: `User-Agent`): (String, String) = ("http.request.user_agent", `User-Agent`.headerInstance.value(userAgent))
    def requestContentLength(cl: Long): (String, String) = ("http.request.content_length", cl.show)
    def responseContentLength(cl: Long): (String, String) = ("http.response.content_length", cl.show)
    def retryCount(i: Int): (String, String)= ("http.retry_count", i.show)
    def peerIp(ip: IpAddress): (String, String) = ("net.peer.ip", ip.toString()) // TODO: Check that this is the right way
    def peerPort(port: Port): (String, String) = ("net.peer.port", port.value.show)

    def logKind(logKind: String) = ("http.kind", logKind)
    def accessTime(duration: FiniteDuration) = ("http.access_time", duration.toMillis.toString)
    def headersDuration(duration: FiniteDuration) = ("http.duration_ms", duration.toMillis.toString())
    def bodyDuration(duration: FiniteDuration) = ("http.duration_body_ms", duration.toMillis.toString())
    def outcome[F[_], E, A](outcome: Outcome[F, E, A]): (String, String) = {
      outcome match {
        case Outcome.Canceled() => "http.exit_case" -> "canceled"
        case Outcome.Errored(_) => "http.exit_case" -> "errored"
        case Outcome.Succeeded(_) => "http.exit_case" -> "succeeded"
      }
    }

  }

  object Client {
    def peerName(uri: Uri): Option[(String, String)] = uri.host.map(h => "net.peer.name" -> h.value)
  }

  object Server {
    def serverName(s: String): (String, String) = ("http.server_name", s)
    // The route template. Since http4s uses unapplies by default this is non-trivial.
    // Accept-List for segements are better to get coherent routes, but block-list segments
    // take much less effort while allowing unexpected variables. We will provide options
    // for this
    def route(s: String): (String, String) = ("http.route", s)
    // This is either the net ip, OR the x-forwarded for depending on what is available.
    def clientIp(ip: IpAddress): (String, String) = ("http.client_ip", ip.toString())
  }


  object Headers {

    def request(headers: Headers, s: Set[CIString]): Map[String, String] =
      generic(headers, s, "request")
    def response(headers: Headers, s: Set[CIString]): Map[String, String] =
      generic(headers, s, "response")

    private def generic(headers: Headers, s: Set[CIString], messageType: String): Map[String, String] = {
      headers.headers
        .groupBy(r => (r.name))
        .map{
          case (name, list) =>
            val key = "http." ++ messageType ++ ".headers." ++ name.toString.toLowerCase
            if (s.contains(name)) (key, list.map(_.value).mkString(", "))
            else (key, "<REDACTED>")
        }
    }

    lazy val defaultHeadersAllowed = Set(
      "WWW-Authenticate", "Proxy-Authenticate",
      "Age", "Cache-Control", "Clear-Site-Data", "Expires", "Pragma", "Warning",
      "Accept-CH", "Accept-CH-Liftetime", "Early-Data", "Device-Memory", "Save-Data", "Viewport-Width", "Width",
      "Last-Modified", "ETag", "If-Match", "If-None-Match", "If-Modified-Since", "If-Unmodified-Since", "Vary",
      "Connection", "Keep-Alive",
      "Accept", "Accept-Charset", "Accept-Encoding", "Accept-Language",
      "Expect", "Max-Forwards",

      "Access-Control-Allow-Origin",
      "Access-Control-Allow-Credentials",
      "Access-Control-Allow-Headers",
      "Access-Control-Expose-Methods",
      "Access-Control-Max-Age",
      "Access-Control-Request-Headers",
      "Access-Control-Request-Method",
      "Origin",
      "Timing-Allow-Origin",

      "DNT", "Tk",
      "Content-Disposition",
      "Content-Length", "Content-Type", "Content-Encoding", "Content-Language", "Content-Location",
      "Forwarded", "X-Forwarded-For", "X-Forwarded-Host", "X-Forwarded-Proto", "X-Forwarded-Scheme", "X-Forwarded-Port", "Via",
      "Location",
      "From", "Host", "Referer", "Referer-Policy", "User-Agent",
      "Allow", "Server",
      "Accept-Ranges", "Range", "If-Range", "Content-Range",
      "Deprecation",

      "Cross-Origin-Embedder-Policy",
      "Cross-Origin-Opener-Policy",
      "Cross-Origin-Resource-Policy",
      "Content-Security-Policy",
      "Content-Security-Policy-Report-Only",
      "Expect-CT",
      "Feature-Policy",
      "Strict-Transport-Security",
      "X-Content-Type-Options",
      "X-Download-Options",
      "X-Frame-Options",
      "X-Permitted-Cross-Domain-Policies",
      "X-Powered-By",
      "X-XSS-Protection",

      "Public-Key-Pins", "Public-Key-Pins-Report-Only",
      "Sec-Fetch-Site", "Sec-Fetch-Mode", "Sec-Fetch-User", "Sec-Fetch-Dest",

      "Transfer-Encoding", "TE", "Trailer",

      "Alt-Svc",
      "Date",
      "Large-Allocation",
      "Link",
      "Retry-After",
      "Server-Timing",
      "SourceMap",
      "X-SourceMap",
      "Upgrade",
      "X-DNS-Prefetch-Control",
      "X-Request-Id",

      "Sec-CH-UA",
      "Sec-CH-UA-Arch",
      "Sec-CH-UA-Bitness",
      "Sec-CH-UA-Full-Version-List",
      "Sec-CH-UA-Full-Version",
      "Sec-CH-UA-Mobile",
      "Sec-CH-UA-Model",
      "Sec-CH-UA-Platform",
      "Sec-CH-UA-Platform-Version",

      "X-Real-Ip",
      "X-Scheme",

      "X-Request-Start",
      "X-Runtime",

      "B3",
      "X-B3-Sampled",
      "X-B3-SpanId",
      "X-B3-TraceId",

    ).map(CIString(_))
  }
}