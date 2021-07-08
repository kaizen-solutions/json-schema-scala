package io.kaizensolutions.jsonschema

final case class Labelled(name: String, document: JsonSchemaDocument) { self =>
  def mapSchema(f: PartialFunction[JsonSchemaDocument, JsonSchemaDocument]): Labelled =
    copy(document = f(document))

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
