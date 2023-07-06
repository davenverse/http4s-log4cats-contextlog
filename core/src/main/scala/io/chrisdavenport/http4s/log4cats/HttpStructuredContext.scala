package io.chrisdavenport.http4s.log4cats


import org.http4s._
import org.typelevel.ci.CIString
import org.http4s.headers._
import cats.syntax.all._
import com.comcast.ip4s._


object HttpStructuredContext {

  object Common {
    def method(m: Method): (String, String) = ("http.method", m.name)
    def url(url: Uri): (String, String)= ("http.url", url.renderString)
    def target(url: Uri): (String, String) = ("http.target", url.copy(scheme = None, authority = None).renderString)
    def host(host: org.http4s.headers.Host): (String, String) = ("http.host", org.http4s.headers.Host.headerInstance.value(host))
    def scheme(scheme: Uri.Scheme): (String, String) = ("http.scheme", scheme.value)
    
    def status(status: Status): (String, String) = ("http.status_code", status.code.show)
    // Need to check both request and response in case negotiation happens
    def flavor(httpVersion: HttpVersion): (String, String) = ("http.flavor", httpVersion.major.toString() ++ "." ++ httpVersion.minor.toString())
    def userAgent(userAgent: `User-Agent`): (String, String) = ("http.user_agent", `User-Agent`.headerInstance.value(userAgent))
    def requestContentLength(cl: Long): (String, String) = ("http.request_content_length", cl.show)
    def responseContentLength(cl: Long): (String, String) = ("http.response_content_length", cl.show)
    def retryCount(i: Int): (String, String)= ("http.retry_count", i.show)
    def peerIp(ip: IpAddress): (String, String) = ("net.peer.ip", ip.toString()) // TODO: Check that this is the right way
    def peerPort(port: Port): (String, String) = ("net.peer.port", port.value.show)
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
      headers.headers.filter(h => s.contains(h.name))
        .groupBy(r => (r.name))
        .map{
          case (name, list) => ("http." ++ messageType ++ ".header." ++ name.toString.toLowerCase, list.map(_.value).mkString(", "))
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
      "Forwarded", "X-Forwarded-For", "X-Forwarded-Host", "X-Forwarded-Proto", "Via",
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
      "Sec-CH-UA-Platform-Version"
    ).map(CIString(_))
  }
}