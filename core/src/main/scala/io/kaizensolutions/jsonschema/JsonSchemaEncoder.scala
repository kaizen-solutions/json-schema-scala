package io.kaizensolutions.jsonschema

/**
 * A typeclass that describes how to convert a Scala data type into a JSON
 * Schema representation in Scala
 * @tparam A
 *   is the Scala data type
 */
trait JsonSchemaEncoder[A] {
  def encode: JsonSchemaDocument
}

object JsonSchemaEncoder extends JsonSchemaEncoderMagnoliaDerivation {
  def apply[A](implicit encoder: JsonSchemaEncoder[A]): JsonSchemaEncoder[A] = encoder

  def simple[A](primType: JsonSchema.Primitive): JsonSchemaEncoder[A] =
    new JsonSchemaEncoder[A] {
      override def encode: JsonSchemaDocument =
        JsonSchemaDocument(
          schema = primType,
          id = primType.render
        )
    }

  implicit val nullSchemaEncoder: JsonSchemaEncoder[Null] =
    simple[Null](JsonSchema.Primitive.Null)

  implicit val booleanSchemaEncoder: JsonSchemaEncoder[Boolean] =
    simple[Boolean](JsonSchema.Primitive.Bool)

  implicit val stringJsonSchemaEncoder: JsonSchemaEncoder[String] =
    simple[String](JsonSchema.Primitive.Str(None, None))

  private def numericEncoder[A](numericType: JsonSchema.Primitive.NumericType): JsonSchemaEncoder[A] =
    simple[A](JsonSchema.Primitive.Numeric(numericType, None, None))

  implicit val intJsonSchemaEncoder: JsonSchemaEncoder[Int] =
    numericEncoder[Int](JsonSchema.Primitive.NumericType.Int)

  implicit val shortJsonSchemaEncoder: JsonSchemaEncoder[Short] =
    numericEncoder[Short](JsonSchema.Primitive.NumericType.Num)

  implicit val longJsonSchemaEncoder: JsonSchemaEncoder[Long] =
    numericEncoder[Long](JsonSchema.Primitive.NumericType.Num)

  implicit val doubleJsonSchemaEncoder: JsonSchemaEncoder[Double] =
    numericEncoder[Double](JsonSchema.Primitive.NumericType.Num)

  implicit val floatJsonSchemaEncoder: JsonSchemaEncoder[Float] =
    numericEncoder[Float](JsonSchema.Primitive.NumericType.Num)

  implicit val byteJsonSchemaEncoder: JsonSchemaEncoder[Byte] =
    numericEncoder[Byte](JsonSchema.Primitive.NumericType.Num)

  implicit def optionJsonSchemaEncoder[A](implicit encoder: JsonSchemaEncoder[A]): JsonSchemaEncoder[Option[A]] =
    new JsonSchemaEncoder[Option[A]] {
      override def encode: JsonSchemaDocument =
        encoder.encode.copy(required = false)
    }

  implicit def seqJsonSchemaEncoder[A](implicit encoder: JsonSchemaEncoder[A]): JsonSchemaEncoder[Seq[A]] =
    new JsonSchemaEncoder[Seq[A]] {
      override def encode: JsonSchemaDocument =
        JsonSchemaDocument(
          id = encoder.encode.id ++ ".Array",
          schema = JsonSchema.Arr(encoder.encode)
        )
    }
}
