package io.kaizensolutions.jsonschema

import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.{Codec, Decoder, Encoder, Json}
import io.kaizensolutions.jsonschema.annotations.Annotations._
import io.kaizensolutions.jsonschema.renderers.circe._

@title("A representation of a Person")
final case class Person(
  @description("age of the person in years") @exclusiveMinimum(0) @minimum(1) @maximum(200) age: Option[Int],
  @description("full name of the person") @minimumLength(0) @maximumLength(150) name: String,
  device: Device
)
object Person {
  implicit val personSchemaEncoder: JsonSchemaEncoder[Person] = JsonSchemaEncoder.derived[Person]
}

final case class Reading(
  @description("poll frequency") pollFreq: Long,
  last: Option[Int]
)
object Reading {
  implicit val readingSchemaEncoder: JsonSchemaEncoder[Reading] = JsonSchemaEncoder.derived[Reading]
}

sealed trait Device
object Device {
  final private case class Simple(id: Int, reading: Reading)       extends Device
  final private case class Cluster(groupId: Int, reading: Reading) extends Device

  def cluster(groupId: Int, reading: Reading): Device = Cluster(groupId, reading)
  def simple(id: Int, reading: Reading): Device       = Simple(id, reading)

  implicit val deviceSchemaEncoder: JsonSchemaEncoder[Device] = JsonSchemaEncoder.derived[Device]

  implicit val deviceCodec: Codec[Device] = {
    val decoder: Decoder[Device] = cursor =>
      for {
        scalaType <- cursor.downField("scalaType").as[String]
        result    <- if (scalaType == "Simple") cursor.as[Simple] else cursor.as[Cluster]
      } yield result

    val encoder: Encoder[Device] = {
      case s: Simple  => s.asJson.mapObject(_.add("scalaType", Json.fromString("Simple")))
      case c: Cluster => c.asJson.mapObject(_.add("scalaType", Json.fromString("Cluster")))
    }

    Codec.from(decoder, encoder)
  }
}

object Example {
  def main(args: Array[String]): Unit =
    println {
      JsonSchemaEncoder[Person].encode
        .withAdditionalPropertiesNested(false)
        .withDiscriminator("scalaType")
        .toJson()
        .spaces2
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
