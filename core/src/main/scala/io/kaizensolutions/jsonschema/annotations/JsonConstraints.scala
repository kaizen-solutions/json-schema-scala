package io.kaizensolutions.jsonschema.annotations

import io.kaizensolutions.jsonschema.JsonSchema

private[jsonschema] object JsonConstraints {
  def applyConstraints(annotations: Seq[Any], schema: JsonSchema): JsonSchema =
    schema match {
      case num: JsonSchema.Primitive.Numeric =>
        num.copy(
          multipleOf = getMultipleOf(annotations),
          rangeConstraints = JsonSchema.Primitive
            .NumericRangeConstraints(
              minimum = getMinimum(annotations),
              exclusiveMinimum = getExclMinimum(annotations),
              maximum = getMaximum(annotations),
              exclusiveMaximum = getExclMaximum(annotations)
            )
            .normalize
        )

      case str: JsonSchema.Primitive.Str =>
        str.copy(lengthConstraint =
          JsonSchema.Primitive
            .StrLengthConstraint(
              min = getMinLength(annotations),
              max = getMaxLength(annotations)
            )
            .normalize
        )

      case ignore =>
        ignore
    }

  def getTitle(in: Seq[Any]): Option[String] =
    in.collectFirst {
      case e if e.isInstanceOf[Annotations.title] => e.asInstanceOf[Annotations.title].text
    }

  def getDescription(in: Seq[Any]): Option[String] =
    in.collectFirst {
      case e if e.isInstanceOf[Annotations.description] => e.asInstanceOf[Annotations.description].text
    }

  private def getMultipleOf(in: Seq[Any]): Option[Double] =
    in.collectFirst {
      case e if e.isInstanceOf[Annotations.multipleOf] => e.asInstanceOf[Annotations.multipleOf].number
    }

  private def getMinimum(in: Seq[Any]): Option[Double] =
    in.collectFirst {
      case e if e.isInstanceOf[Annotations.minimum] => e.asInstanceOf[Annotations.minimum].number
    }

  private def getMaximum(in: Seq[Any]): Option[Double] =
    in.collectFirst {
      case e if e.isInstanceOf[Annotations.maximum] => e.asInstanceOf[Annotations.maximum].number
    }

  private def getMinLength(in: Seq[Any]): Option[Int] =
    in.collectFirst {
      case e if e.isInstanceOf[Annotations.minimumLength] => e.asInstanceOf[Annotations.minimumLength].number
    }

  private def getMaxLength(in: Seq[Any]): Option[Int] =
    in.collectFirst {
      case e if e.isInstanceOf[Annotations.maximumLength] => e.asInstanceOf[Annotations.maximumLength].number
    }

  private def getExclMinimum(in: Seq[Any]): Option[Double] =
    in.collectFirst {
      case e if e.isInstanceOf[Annotations.exclusiveMinimum] => e.asInstanceOf[Annotations.exclusiveMinimum].number
    }

  private def getExclMaximum(in: Seq[Any]): Option[Double] =
    in.collectFirst {
      case e if e.isInstanceOf[Annotations.exclusiveMaximum] => e.asInstanceOf[Annotations.exclusiveMaximum].number
    }
}
