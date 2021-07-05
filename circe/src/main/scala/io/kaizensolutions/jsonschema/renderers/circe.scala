package io.kaizensolutions.jsonschema.renderers

import io.circe.Json
import io.circe.syntax._
import io.kaizensolutions.jsonschema._

object circe {
  implicit class CirceJsonSyntax(in: JsonSchemaDocument) {
    def toJson: Json = Json.fromFields(render(in))
  }

  private def render(in: JsonSchemaDocument, topLevel: Boolean = true): Seq[(String, Json)] = {
    val id         = if (topLevel) Seq("$id" := in.id) else Seq.empty
    val properties = renderSchema(in.schema)
    val definitions =
      if (in.definitions.isEmpty) Seq.empty
      else
        Seq("definitions" := Json.fromFields(in.definitions.map(renderDefinition)))

    val title       = in.title.map(t => "title" := t).toSeq
    val description = in.description.map(d => "description" := d).toSeq

    val version =
      if (topLevel) in.version match {
        case SchemaVersion.Draft7 => Seq("$schema" := "http://json-schema.org/draft-07/schema#")
      }
      else Seq.empty

    version ++ id ++ properties ++ definitions ++ title ++ description
  }

  private def renderSchema(in: JsonSchema): Seq[(String, Json)] = in match {
    case prim @ JsonSchema.Primitive(_) => renderPrimitive(prim)

    case ref @ JsonSchema.Reference(_) => renderRef(ref)

    case n: JsonSchema.Null => renderNull(n)

    case p @ JsonSchema.Obj.Product(_, _, _) => renderProduct(p)

    case s @ JsonSchema.Obj.Sum(_) => renderSum(s)

    case a @ JsonSchema.Arr(_) => renderArr(a)
  }

  private def renderPrimitive(in: JsonSchema.Primitive): Seq[(String, Json)] =
    List("type" := in.typeName)

  private def renderRef(in: JsonSchema.Reference): Seq[(String, Json)] =
    List("$ref" := in.value)

  private def renderNull(in: JsonSchema.Null): Seq[(String, Json)] =
    List("type" := "\"null\"")

  private def renderProduct(in: JsonSchema.Obj.Product): Seq[(String, Json)] =
    List(
      "type" := "object",
      "properties" := Json.fromFields(
        in.properties.map { case Labelled(key, value) =>
          key -> Json.fromFields(render(in = value, topLevel = false))
        }
      ),
      "required" := in.requiredKeys,
      "additionalProperties" := in.additionalProperties
    )

  private def renderSum(in: JsonSchema.Obj.Sum): Seq[(String, Json)] =
    List(
      "oneOf" := Json.fromValues(
        in.terms
          .map(payload => render(payload, topLevel = false))
          .map(Json.fromFields)
      )
    )

  private def renderArr(in: JsonSchema.Arr): Seq[(String, Json)] =
    List(
      "type" := "array",
      "items" := Json.fromFields(render(in.itemsRep, topLevel = false))
    )

  private def renderDefinition(in: Labelled): (String, Json) =
    in.name -> Json.fromFields(
      ("$id" := in.schema.id) +: render(in.schema, topLevel = false)
    )
}
