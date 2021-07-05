package io.kaizensolutions.jsonschema

/**
 * A pure datatype that describes a JSON Schema in Scala
 *
 * @param version is the JSON Schema version
 * @param schema is the type of schema (i.e. string, int, boolean, object, etc.)
 * @param id is the id of the JSON Schema document
 * @param title is the title of the JSON Schema document
 * @param description is the description of the JSON Schema document
 * @param definitions contains any data types defined by the JSON Schema
 * @param required are a list of all the required fields
 */
final case class JsonSchemaDocument(
  version: SchemaVersion = SchemaVersion.Draft7,
  schema: JsonSchema,
  id: String,
  title: Option[String] = None,
  description: Option[String] = None,
  definitions: Set[Labelled] = Set.empty,
  required: Boolean = true
) { self =>

  def withTitle(title: String): JsonSchemaDocument = copy(title = Some(title))

  def withDescription(description: String): JsonSchemaDocument = copy(description = Some(description))

  def isPrimitive: Boolean = schema match {
    case JsonSchema.Primitive(_) => true
    case JsonSchema.Null         => true
    case _                       => false
  }

  def isCaseObject: Boolean = schema match {
    case _: JsonSchema.Obj.CaseObj => true
    case _                         => false
  }

  def isObject: Boolean = schema match {
    case _: JsonSchema.Obj => true
    case _                 => false
  }

  // Use Optics library instead?
  def modifyProduct(f: JsonSchema.Obj.Product => JsonSchema): JsonSchemaDocument =
    self.copy(
      schema = self.schema match {
        case p: JsonSchema.Obj.Product => f(p)
        case o                         => o
      }
    )

  def transformDown(pf: PartialFunction[JsonSchema, JsonSchema]): JsonSchemaDocument = {
    val top: JsonSchema = if (pf.isDefinedAt(schema)) pf(schema) else schema
    val res = top match {
      case p @ (JsonSchema.Primitive(_) | JsonSchema.Reference(_) | JsonSchema.Null | JsonSchema.Obj.CaseObj(_)) =>
        p

      case JsonSchema.Obj.Product(properties, requiredKeys, additionalProperties) =>
        JsonSchema.Obj.Product(
          properties.map(_.mapSchema(_.transformDown(pf))),
          requiredKeys,
          additionalProperties
        )

      case JsonSchema.Obj.Sum(terms) =>
        JsonSchema.Obj.Sum(terms.map(_.transformDown(pf)))

      case JsonSchema.Arr(items) =>
        JsonSchema.Arr(items.transformDown(pf))
    }

    self.copy(schema = res)
  }

  def withAdditionalProperties(value: Boolean): JsonSchemaDocument =
    modifyProduct(_.copy(additionalProperties = value))

  def withAdditionalPropertiesNested(value: Boolean): JsonSchemaDocument = {
    val change: PartialFunction[JsonSchema, JsonSchema] = { case p @ JsonSchema.Obj.Product(_, _, _) =>
      p.copy(additionalProperties = value)
    }
    // propagate change to definitions
    self
      .transformDown(change)
      .copy(definitions = definitions.map(_.mapSchema(payload => payload.transformDown(change))))
  }

  // I'm not really sure how I feel about this
  def withDiscriminator(value: String): JsonSchemaDocument = {
    // Modify all products contained within a sum
    val change: PartialFunction[JsonSchema, JsonSchema] = { case sum @ JsonSchema.Obj.Sum(_) =>
      val result =
        sum.terms.map(_.transformDown { case p @ JsonSchema.Obj.Product(_, _, _) =>
          val discriminator =
            Labelled(value, JsonSchemaDocument(id = "string", schema = JsonSchema.Primitive("string")))
          p.copy(properties = discriminator +: p.properties)
        })

      sum.copy(result)
    }

    self
      .transformDown(change)
      .copy(definitions = definitions.map(_.mapSchema(_.transformDown(change))))
  }

  override def toString: String =
    self.productElementNames
      .zip(self.productIterator)
      .map { case (key, value) =>
        s"\t$key=$value"
      }
      .mkString(
        start = "JsonSchemaPayload(" + System.lineSeparator(),
        sep = "," + System.lineSeparator(),
        end = System.lineSeparator + ")"
      )
}

// See https://json-schema.org/understanding-json-schema/reference/index.html
sealed trait JsonSchema
object JsonSchema {
  final case class Primitive(typeName: String) extends JsonSchema

  final case class Reference(value: String) extends JsonSchema

  type Null = Null.type
  final case object Null extends JsonSchema

  sealed trait Obj extends JsonSchema
  object Obj {
    final case class CaseObj(allowedValues: Set[String]) extends Obj {
      def ++(that: CaseObj): CaseObj = copy(allowedValues ++ that.allowedValues)
    }

    final case class Product(
      properties: Seq[Labelled],
      requiredKeys: Seq[String],
      additionalProperties: Boolean = true
    )                                                    extends Obj
    final case class Sum(terms: Seq[JsonSchemaDocument]) extends Obj
  }

  final case class Arr(itemsRep: JsonSchemaDocument) extends JsonSchema
}

sealed trait SchemaVersion
object SchemaVersion {
  case object Draft7 extends SchemaVersion
}

final case class Labelled(name: String, schema: JsonSchemaDocument) { self =>
  def mapSchema(f: PartialFunction[JsonSchemaDocument, JsonSchemaDocument]): Labelled =
    copy(schema = f(schema))

  override def toString: String =
    self.productElementNames
      .zip(self.productIterator)
      .map { case (key, value) =>
        s"\t$key=$value"
      }
      .mkString(
        start = "Labelled(" + System.lineSeparator(),
        sep = "," + System.lineSeparator(),
        end = System.lineSeparator + ")"
      )
}
