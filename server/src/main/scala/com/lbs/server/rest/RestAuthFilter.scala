package com.lbs.server.rest

import jakarta.servlet.FilterChain
import jakarta.servlet.http.{HttpServletRequest, HttpServletResponse}
import org.springframework.beans.factory.annotation.Value
import org.springframework.stereotype.Component
import org.springframework.web.filter.OncePerRequestFilter

@Component
class RestAuthFilter extends OncePerRequestFilter {

  @Value("${REST_SECRET:}")
  private var configuredSecret: String = ""

  override protected def doFilterInternal(
    request: HttpServletRequest,
    response: HttpServletResponse,
    filterChain: FilterChain
  ): Unit = {
    val path = request.getRequestURI
    val isHealth = path.endsWith("/api/v1/health")
    val supplied = Option(request.getHeader("X-LuxMed-Secret")).getOrElse("")
    if (isHealth || (configuredSecret.nonEmpty && supplied == configuredSecret)) {
      filterChain.doFilter(request, response)
    } else if (configuredSecret.isEmpty) {
      response.sendError(HttpServletResponse.SC_SERVICE_UNAVAILABLE, "REST_SECRET is not configured")
    } else {
      response.sendError(HttpServletResponse.SC_UNAUTHORIZED, "Invalid sidecar secret")
    }
  }
}
