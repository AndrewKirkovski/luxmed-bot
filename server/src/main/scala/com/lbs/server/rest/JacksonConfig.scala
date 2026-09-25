package com.lbs.server.rest

import org.springframework.context.annotation.Configuration

// Spring Boot 4 owns the Jackson 3 JsonMapper. Keeping a Jackson 2
// ObjectMapper bean here would prevent MVC from using the Boot 4 converter.
@Configuration
class JacksonConfig
