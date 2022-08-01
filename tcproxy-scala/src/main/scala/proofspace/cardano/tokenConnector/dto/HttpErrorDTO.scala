package proofspace.cardano.tokenConnector.dto

import com.github.plokhotnyuk.jsoniter_scala.macros._
import com.github.plokhotnyuk.jsoniter_scala.core._

import sttp.tapir.Schema
import sttp.tapir.generic.auto._

case class HttpErrorDTO(code: String, message: String, details: Option[String]=None)

object HttpErrorDTO {

  implicit val jsonCodec: JsonValueCodec[HttpErrorDTO] = JsonCodecMaker.make
  implicit val tapirSchema: Schema[HttpErrorDTO] = Schema.derived

}