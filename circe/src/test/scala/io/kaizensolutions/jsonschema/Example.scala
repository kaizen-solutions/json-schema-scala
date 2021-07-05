package io.kaizensolutions.jsonschema

import io.circe.Codec
import io.circe.generic.auto._
import io.circe.generic.extras._
import io.circe.generic.extras.semiauto._
import io.circe.syntax._

import io.kaizensolutions.jsonschema.annotations._
import io.kaizensolutions.jsonschema.renderers.circe._

@title("A representation of a Person")
final case class Person(
  @description("age of the person in years") age: Option[Int],
  @description("full name of the person") name: String,
  device: Device
)

final case class Reading(
  @description("poll frequency") pollFreq: Long,
  last: Option[Int]
)
sealed trait Device
object Device {
  private final case class Simple(id: Int, reading: Reading)       extends Device
  private final case class Cluster(groupId: Int, reading: Reading) extends Device

  def cluster(groupId: Int, reading: Reading): Device = Cluster(groupId, reading)

  def simple(id: Int, reading: Reading): Device = Simple(id, reading)

  implicit val deviceCodec: Codec[Device] = {
    implicit val configuration: Configuration =
      Configuration.default.withSnakeCaseConstructorNames
        .withDiscriminator("scalaType")

    deriveConfiguredCodec[Device]
  }
}

object Example {

  def main(args: Array[String]): Unit = {
    println {
      JsonSchemaEncoder[Person].encode
//        .withAdditionalPropertiesNested(false)
//        .withDiscriminator("scalaType")
        .toJson.spaces2
    }

    println {
      Person(
        Some(30),
        "Cal",
        Device.simple(
          1,
          Reading(1000, Some(999))
        )
      ).asJson.spaces2
    }
  }
}
