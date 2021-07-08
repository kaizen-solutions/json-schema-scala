package io.kaizensolutions.jsonschema

// See https://json-schema.org/understanding-json-schema/reference/index.html
sealed trait JsonSchema
object JsonSchema {
  sealed trait Primitive extends JsonSchema { self =>
    def render: String = self match {
      case Primitive.Str(_) => "string"
      case Primitive.Bool   => "boolean"
      case Primitive.Null   => "null"
      case Primitive.Numeric(numericType, _, _) =>
        numericType match {
          case Primitive.NumericType.Int => "integer"
          case Primitive.NumericType.Num => "number"
        }
    }
  }
  object Primitive {
    // TODO: Model out further
    final case class Str(constant: Option[String] = None) extends Primitive {
      def withConstant(cst: String): Str = Str(Some(cst))
    }

    type Bool = Bool.type
    final case object Bool extends Primitive

    type Null = Null.type
    final case object Null extends Primitive

    final case class Numeric(
      numericType: NumericType,
      rangeConstraints: Option[RangeConstraints],
      multipleOf: Option[Double]
    ) extends Primitive { self =>
      def withMultipleOf(m: Double): Primitive = self.copy(multipleOf = Some(m))
    }

    sealed trait NumericType
    object NumericType {
      final case object Int extends NumericType
      final case object Num extends NumericType
    }

    final case class RangeConstraints(
      minimum: Option[Double],
      exclusiveMinimum: Option[Double],
      maximum: Option[Double],
      exclusiveMaximum: Option[Double]
    ) { self =>
      def normalize: Option[RangeConstraints] =
        minimum
          .orElse(exclusiveMinimum)
          .orElse(maximum)
          .orElse(exclusiveMinimum)
          .map(_ => self)
    }
  }

  final case class Reference(value: String) extends JsonSchema

  sealed trait Obj extends JsonSchema
  object Obj {
    final case class CaseObj(allowedValues: Set[String], belongsToSum: Option[String] = None) extends Obj {
      def ++(that: CaseObj): CaseObj = copy(allowedValues = allowedValues ++ that.allowedValues)
    }

    final case class Product(
      typeName: String,
      properties: Seq[Labelled],
      requiredKeys: Seq[String],
      additionalProperties: Boolean = true,
      belongsToSum: Option[String] = None
    )                                                                      extends Obj
    final case class Sum(typeName: String, terms: Seq[JsonSchemaDocument]) extends Obj
  }

  final case class Arr(itemsRep: JsonSchemaDocument) extends JsonSchema
}
