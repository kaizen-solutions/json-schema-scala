package io.kaizensolutions.jsonschema

// See https://json-schema.org/understanding-json-schema/reference/index.html
sealed trait JsonSchema
object JsonSchema {
  sealed trait Primitive extends JsonSchema { self =>
    def render: String = self match {
      case Primitive.Str(_, _) => "string"
      case Primitive.Bool      => "boolean"
      case Primitive.Null      => "null"
      case Primitive.Numeric(numericType, _, _) =>
        numericType match {
          case Primitive.NumericType.Int => "integer"
          case Primitive.NumericType.Num => "number"
        }
    }
  }
  object Primitive {
    // TODO: Model out further
    final case class Str(constant: Option[String] = None, lengthConstraint: Option[StrLengthConstraint] = None)
        extends Primitive {
      def withConstant(cst: String): Str = copy(constant = Some(cst))
    }
    final case class StrLengthConstraint(min: Option[Int], max: Option[Int]) { self =>
      def normalize: Option[StrLengthConstraint] =
        min.orElse(max).map(_ => self)
    }

    type Bool = Bool.type
    case object Bool extends Primitive

    type Null = Null.type
    case object Null extends Primitive

    final case class Numeric(
      numericType: NumericType,
      rangeConstraints: Option[NumericRangeConstraints],
      multipleOf: Option[Double]
    ) extends Primitive { self =>
      def withMultipleOf(m: Double): Primitive = self.copy(multipleOf = Some(m))
    }

    sealed trait NumericType
    object NumericType {
      case object Int extends NumericType
      case object Num extends NumericType
    }

    final case class NumericRangeConstraints(
      minimum: Option[Double],
      exclusiveMinimum: Option[Double],
      maximum: Option[Double],
      exclusiveMaximum: Option[Double]
    ) { self =>
      def normalize: Option[NumericRangeConstraints] =
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
    ) extends Obj
    final case class Sum(typeName: String, terms: Seq[JsonSchemaDocument]) extends Obj
  }

  final case class Arr(itemsRep: JsonSchemaDocument) extends JsonSchema
}
