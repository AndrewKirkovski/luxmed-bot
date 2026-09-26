package com.lbs.server.rest

import jakarta.servlet.FilterChain
import jakarta.servlet.http.{HttpServletRequest, HttpServletResponse}
import org.springframework.beans.factory.annotation.Value
import org.springframework.stereotype.Component
import org.springframework.web.filter.OncePerRequestFilter

import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest
import java.util.HexFormat
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

object RestAuthFilter {
  def effectiveSecret(explicit: String, encryptionSecret: String): String = {
    if (explicit.nonEmpty) explicit
    else if (encryptionSecret.nonEmpty) {
      val mac = Mac.getInstance("HmacSHA256")
      mac.init(new SecretKeySpec(encryptionSecret.getBytes(UTF_8), "HmacSHA256"))
      HexFormat.of().formatHex(mac.doFinal("luxmed-rest-auth-v1".getBytes(UTF_8)))
    } else ""
  }
}

@Component
class RestAuthFilter extends OncePerRequestFilter {

  @Value("${REST_SECRET:}")
  private var configuredSecret: String = ""

  @Value("${SECURITY_SECRET:}")
  private var encryptionSecret: String = ""

  override protected def doFilterInternal(
    request: HttpServletRequest,
    response: HttpServletResponse,
    filterChain: FilterChain
  ): Unit = {
    val path = request.getRequestURI
    val isHealth = path == "/api/v1/health" && request.getMethod == "GET"
    val secret = RestAuthFilter.effectiveSecret(configuredSecret, encryptionSecret)
    val supplied = Option(request.getHeader("X-LuxMed-Secret")).getOrElse("")
    if (isHealth || (secret.nonEmpty && MessageDigest.isEqual(supplied.getBytes(UTF_8), secret.getBytes(UTF_8)))) {
      filterChain.doFilter(request, response)
    } else if (secret.isEmpty) {
      response.sendError(HttpServletResponse.SC_SERVICE_UNAVAILABLE, "REST authentication is not configured")
    } else {
      response.sendError(HttpServletResponse.SC_UNAUTHORIZED, "Invalid sidecar secret")
    }
  }
}
