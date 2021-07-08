package io.kaizensolutions.jsonschema

sealed trait SchemaVersion
object SchemaVersion {
  case object Draft7 extends SchemaVersion
}
