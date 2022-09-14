



resolvers += "IOHK repo" at "https://maven.pkg.github.com/input-output-hk/atala-prism-sdk"
credentials += Credentials( file("iohk.credentials") )

resolvers += "Kotlinx coroutines" at "https://maven.pkg.jetbrains.space/public/p/kotlinx-coroutines/maven/"


name := "token-search"
scalaVersion := "3.2.0"
libraryDependencies ++= Seq(
 

  "com.softwaremill.sttp.tapir" %% "tapir-core" % "1.1.0",
  // -- incompatible with org.scala-lang.modules:scala-java8-compat_2.13:1.0.0 from akka for scala-2.13
  //"com.softwaremill.sttp.tapir" %% "tapir-armeria-server" % "1.1.0",
  "com.softwaremill.sttp.tapir" %% "tapir-netty-server" % "1.1.0",
 
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

  ("solutions.iog" %% "psg-cardano-wallet-api" % "0.3.4").cross(CrossVersion.for3Use2_13),
   "com.bloxbean.cardano" % "cardano-client-lib" % "0.2.0"


)


