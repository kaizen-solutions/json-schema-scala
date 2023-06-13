package io.kaizensolutions.jsonschema

/**
 * A pure datatype that describes a JSON Schema in Scala
 *
 * @param schema
 *   is the type of schema (i.e. string, int, boolean, object, etc.)
 * @param id
 *   is the id of the JSON Schema document
 * @param title
 *   is the title of the JSON Schema document
 * @param description
 *   is the description of the JSON Schema document
 * @param definitions
 *   contains any data types defined by the JSON Schema
 * @param required
 *   are a list of all the required fields
 */
final case class JsonSchemaDocument(
  schema: JsonSchema,
  id: String,
  title: Option[String] = None,
  description: Option[String] = None,
  definitions: Set[Labelled] = Set.empty,
  required: Boolean = true
) { self =>

  def shortName: String = id.split('.').lastOption.getOrElse(id)

  def withTitle(title: String): JsonSchemaDocument = copy(title = Some(title))

  def withDescription(description: String): JsonSchemaDocument = copy(description = Some(description))

  def withoutDefinitions: JsonSchemaDocument = copy(definitions = Set.empty)

  def belongsTo(sumId: String): JsonSchemaDocument =
    copy(schema = schema match {
      case caseObj: JsonSchema.Obj.CaseObj => caseObj.copy(belongsToSum = Some(sumId))
      case product: JsonSchema.Obj.Product => product.copy(belongsToSum = Some(sumId))
      case arr @ JsonSchema.Arr(items)     => arr.copy(items.belongsTo(sumId))
      case o                               => o
    })

  def isPrimitive: Boolean = schema match {
    case _: JsonSchema.Primitive => true
    case _                       => false
  }

  def isNumeric: Boolean = schema match {
    case _: JsonSchema.Primitive.Numeric => true
    case _                               => false
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
      case terminalCase @ (
            JsonSchema.Primitive.Bool | JsonSchema.Primitive.Null | JsonSchema.Primitive.Str(_, _) |
            JsonSchema.Primitive.Numeric(_, _, _) | JsonSchema.Reference(_) | JsonSchema.Obj.CaseObj(_, _)
          ) =>
        terminalCase

      case p @ JsonSchema.Obj.Product(_, properties, _, _, _) =>
        p.copy(properties = properties.map(_.mapSchema { case document =>
          document.transformDown(pf)
        }))

      case s @ JsonSchema.Obj.Sum(_, terms) =>
        s.copy(terms = terms.map(_.transformDown(pf)))

      case JsonSchema.Arr(items) =>
        JsonSchema.Arr(items.transformDown(pf))
    }

    self.copy(schema = res)
  }

  def withAdditionalProperties(value: Boolean): JsonSchemaDocument =
    modifyProduct(_.copy(additionalProperties = value))

  def withAdditionalPropertiesNested(value: Boolean): JsonSchemaDocument = {
    val change: PartialFunction[JsonSchema, JsonSchema] = { case p @ JsonSchema.Obj.Product(_, _, _, _, _) =>
      p.copy(additionalProperties = value)
    }
    // propagate change to definitions
    self
      .transformDown(change)
      .copy(definitions = definitions.map(_.mapSchema { case payload => payload.transformDown(change) }))
  }

  // I'm not really sure how I feel about this - maybe this needs to be first-class?
  def withDiscriminator(disc: String): JsonSchemaDocument = {
    // Modify all products contained within a sum
    val change: PartialFunction[JsonSchema, JsonSchema] = {
      case product @ JsonSchema.Obj.Product(typeName, _, _, _, Some(_)) =>
        val primStr = JsonSchema.Primitive.Str(constant = Some(typeName))
        val discriminator = Labelled(
          disc,
          JsonSchemaDocument(
            id = primStr.render,
            schema = primStr
          )
        )
        product.copy(
          requiredKeys = disc +: product.requiredKeys,
          properties = discriminator +: product.properties
        )
    }

    self
      .transformDown(change)
      .copy(definitions = definitions.map(_.mapSchema { case document => document.transformDown(change) }))
  }
}
