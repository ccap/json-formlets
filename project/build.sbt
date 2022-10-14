resolvers := Seq("ccap-artifactory-scala" at "https://repoman.wicourts.gov/artifactory/scala")

externalResolvers := Resolver.defaultLocal +: resolvers.value

addSbtPlugin("gov.wicourts" %% "sbt-bootstrap-plugin" % "3.0.3")

