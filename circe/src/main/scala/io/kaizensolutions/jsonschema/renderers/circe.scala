package io.kaizensolutions.jsonschema.renderers

import io.circe.Json
import io.circe.syntax._
import io.kaizensolutions.jsonschema.JsonSchema.Primitive
import io.kaizensolutions.jsonschema._

object circe {
  implicit class CirceJsonSyntax(in: JsonSchemaDocument) {
    def toJson(schemaVersion: SchemaVersion = SchemaVersion.Draft7): Json = Json.fromFields(render(in, schemaVersion))
  }

  private def render(
    document: JsonSchemaDocument,
    schemaVersion: SchemaVersion,
    topLevel: Boolean = true
  ): Seq[(String, Json)] = {
    def renderSchema(in: JsonSchema): Seq[(String, Json)] = in match {
      case prim: JsonSchema.Primitive => renderPrimitive(prim)

      case ref @ JsonSchema.Reference(_) => renderRef(ref)

      case p @ JsonSchema.Obj.Product(_, _, _, _, _) => renderProduct(p)

      case s @ JsonSchema.Obj.Sum(_, _) => renderSum(s)

      case c @ JsonSchema.Obj.CaseObj(_, _) => renderCaseObj(c)

      case a @ JsonSchema.Arr(_) => renderArr(a)
    }

    def renderPrimitive(in: JsonSchema.Primitive): Seq[(String, Json)] = {
      val init = List("type" := in.render)

      val additions = in match {
        case Primitive.Str(constant) =>
          constant.map("constant" := _).toList

        case Primitive.Numeric(_, rangeConstraints, multipleOf) =>
          val multipleC = multipleOf.map("multipleOf" := _).toList
          val rangeC = rangeConstraints.map { cs =>
            cs.minimum.map("minimum" := _).toList ++
              cs.maximum.map("maximum" := _).toList ++
              cs.exclusiveMinimum.map("exclusiveMinimum" := _).toList ++
              cs.exclusiveMaximum.map("exclusiveMaximum" := _).toList
          }.getOrElse(Nil)

          multipleC ++ rangeC

        case Primitive.Bool => Nil
        case Primitive.Null => Nil
      }

      init ++ additions
    }

    def renderRef(in: JsonSchema.Reference): Seq[(String, Json)] =
      List("$ref" := in.value)

    def renderProduct(in: JsonSchema.Obj.Product): Seq[(String, Json)] =
      List(
        "type" := "object",
        "properties" := Json.fromFields(
          in.properties.map { case Labelled(key, value) =>
            key -> Json.fromFields(render(document = value, schemaVersion = schemaVersion, topLevel = false))
          }
        ),
        "required" := in.requiredKeys,
        "additionalProperties" := in.additionalProperties
      )

    def renderSum(in: JsonSchema.Obj.Sum): Seq[(String, Json)] =
      List(
        "oneOf" := Json.fromValues(
          in.terms
            .map(payload => render(payload, schemaVersion = schemaVersion, topLevel = false))
            .map(Json.fromFields)
        )
      )

    def renderCaseObj(in: JsonSchema.Obj.CaseObj): Seq[(String, Json)] =
      List(
        "type" := "string",
        "enum" := in.allowedValues
      )

    def renderArr(in: JsonSchema.Arr): Seq[(String, Json)] =
      List(
        "type" := "array",
        "items" := Json.fromFields(render(in.itemsRep, schemaVersion, topLevel = false))
      )

    def renderDefinition(in: Labelled): (String, Json) =
      in.name -> Json.fromFields(
        ("$id" := in.document.id) +: render(in.document, schemaVersion, topLevel = false)
      )

    val id         = if (topLevel) Seq("$id" := document.id) else Seq.empty
    val properties = renderSchema(document.schema)
    val definitions =
      if (document.definitions.isEmpty) Seq.empty
      else Seq("definitions" := Json.fromFields(document.definitions.map(renderDefinition)))

    val title       = document.title.map(t => "title" := t).toSeq
    val description = document.description.map(d => "description" := d).toSeq

    val version =
      if (topLevel) schemaVersion match {
        case SchemaVersion.Draft7 => Seq("$schema" := "http://json-schema.org/draft-07/schema#")
      }
      else Seq.empty

    version ++ id ++ properties ++ definitions ++ title ++ description
  }
}
