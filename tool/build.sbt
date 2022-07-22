

val tapirVersion = "1.0.0"

resolvers += "IOHK repo" at "https://maven.pkg.github.com/input-output-hk/atala-prism-sdk"
credentials += Credentials( file("iohk.credentials") )

resolvers += "Kotlinx coroutines" at "https://maven.pkg.jetbrains.space/public/p/kotlinx-coroutines/maven/"


name := "token-search"
scalaVersion := "3.1.1"
libraryDependencies ++= Seq(
  ("com.softwaremill.sttp.tapir" %% "tapir-core" % tapirVersion).cross(CrossVersion.for3Use2_13),
  ("com.softwaremill.sttp.tapir" %% "tapir-akka-http-server" % tapirVersion).cross(CrossVersion.for3Use2_13),
  ("com.softwaremill.sttp.tapir" %% "tapir-sttp-client" % tapirVersion).cross(CrossVersion.for3Use2_13) ,
  "com.github.jwt-scala" %% "jwt-core" % "9.0.5",
  "com.geirsson" %% "metaconfig-typesafe-config" % "0.10.0",
  "com.github.rssh" %% "dotty-cps-async" % "0.9.10",
  "org.scalameta" %% "munit" % "0.7.29" % Test,

  "io.iohk.atala" % "prism-crypto" % "v1.3.3",
  "io.iohk.atala" % "prism-identity" % "v1.3.3",
  "io.iohk.atala" % "prism-credentials" % "v1.3.3",
  "io.iohk.atala" % "prism-api" % "v1.3.3",
  "org.jetbrains.kotlin" % "kotlin-stdlib" % "1.6.20",

  ("solutions.iog" %% "psg-cardano-wallet-api" % "0.3.4").cross(CrossVersion.for3Use2_13)


)


