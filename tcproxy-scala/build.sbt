

val akkaVersion = "2.6.19"
val akkaHttpVersion = "10.2.9"


resolvers += "IOHK repo" at "https://maven.pkg.github.com/input-output-hk/atala-prism-sdk"
credentials += Credentials( file("iohk.credentials") )

resolvers += "Kotlinx coroutines" at "https://maven.pkg.jetbrains.space/public/p/kotlinx-coroutines/maven/"


name := "token-search"
scalaVersion := "3.1.1"
libraryDependencies ++= Seq(
 
  ("com.typesafe.akka" %% "akka-actor-typed" % akkaVersion).cross(CrossVersion.for3Use2_13),
  ("com.typesafe.akka" %% "akka-stream" % akkaVersion).cross(CrossVersion.for3Use2_13),
  ("com.typesafe.akka" %% "akka-http" % akkaHttpVersion).cross(CrossVersion.for3Use2_13),
 
  "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core"   % "2.13.38",
  "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "2.13.38" % "compile-internal",


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


