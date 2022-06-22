package io.kaizensolutions.jsonschema

final case class Labelled(name: String, document: JsonSchemaDocument) { self =>
  def mapSchema(f: PartialFunction[JsonSchemaDocument, JsonSchemaDocument]): Labelled =
    copy(document = f(document))
}
