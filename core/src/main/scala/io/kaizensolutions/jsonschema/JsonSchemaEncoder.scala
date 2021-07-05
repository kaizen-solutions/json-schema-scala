package io.kaizensolutions.jsonschema

import magnolia._

import scala.language.experimental.macros

/**
 * A typeclass that describes how to convert a Scala data type into a JSON Schema representation in Scala
 * @tparam A is the Scala data type
 */
trait JsonSchemaEncoder[A] {
  def encode: JsonSchemaDocument
}

object JsonSchemaEncoder {
  def apply[A](implicit encoder: JsonSchemaEncoder[A]): JsonSchemaEncoder[A] = encoder

  def simple[A](typeName: String): JsonSchemaEncoder[A] =
    new JsonSchemaEncoder[A] {
      override def encode: JsonSchemaDocument =
        JsonSchemaDocument(
          schema = JsonSchema.Primitive(typeName),
          id = typeName
        )
    }

  private val nullKey   = "null"
  private val stringKey = "string"
  private val boolKey   = "boolean"
  private val intKey    = "integer"
  private val numKey    = "number"

  implicit val nullSchemaEncoder: JsonSchemaEncoder[Null] =
    simple[Null](nullKey)

  implicit val booleanSchemaEncoder: JsonSchemaEncoder[Boolean] =
    simple[Boolean](boolKey)

  implicit val stringJsonSchemaEncoder: JsonSchemaEncoder[String] =
    simple[String](stringKey)

  implicit val intJsonSchemaEncoder: JsonSchemaEncoder[Int] =
    simple[Int](intKey)

  implicit val shortJsonSchemaEncoder: JsonSchemaEncoder[Short] =
    simple[Short](numKey)

  implicit val longJsonSchemaEncoder: JsonSchemaEncoder[Long] =
    simple[Long](numKey)

  implicit val doubleJsonSchemaEncoder: JsonSchemaEncoder[Double] =
    simple[Double](numKey)

  implicit val floatJsonSchemaEncoder: JsonSchemaEncoder[Float] =
    simple[Float](numKey)

  implicit val byteJsonSchemaEncoder: JsonSchemaEncoder[Byte] =
    simple[Byte](numKey)

  implicit def optionJsonSchemaEncoder[A](implicit encoder: JsonSchemaEncoder[A]): JsonSchemaEncoder[Option[A]] =
    new JsonSchemaEncoder[Option[A]] {
      override def encode: JsonSchemaDocument =
        encoder.encode.copy(required = false)
    }

  implicit def seqJsonSchemaEncoder[A](implicit encoder: JsonSchemaEncoder[A]): JsonSchemaEncoder[Seq[A]] =
    new JsonSchemaEncoder[Seq[A]] {
      override def encode: JsonSchemaDocument =
        JsonSchemaDocument(
          id = encoder.encode.id ++ ".Array",
          schema = JsonSchema.Arr(encoder.encode)
        )
    }

  type Typeclass[T] = JsonSchemaEncoder[T]

  def combine[T](caseClass: CaseClass[Typeclass, T]): Typeclass[T] = new Typeclass[T] {
    override def encode: JsonSchemaDocument =
      if (caseClass.isObject) {
        JsonSchemaDocument(
          id = caseClass.typeName.full,
          schema = JsonSchema.Obj.CaseObj(Set(caseClass.typeName.short))
        )
      } else {
        val primitives: Seq[Labelled] =
          caseClass.parameters.collect {
            case param if param.typeclass.encode.isPrimitive =>
              val description = getDescription(param.annotations)
              val title       = getTitle(param.annotations)
              val key         = param.label
              val tcInstance  = param.typeclass
              Labelled(key, tcInstance.encode.copy(description = description, title = title))
          }

        // converted nested objects to references
        val references =
          caseClass.parameters.collect {
            case param if param.typeclass.encode.isObject =>
              val key        = param.label
              val tcInstance = param.typeclass
              Labelled(
                key,
                JsonSchemaDocument(
                  schema = JsonSchema.Reference(tcInstance.encode.id),
                  id = tcInstance.encode.id
                )
              )
          }

        // pull definitions inside nested objects into the top level (this will happen recursively)
        val definitions =
          caseClass.parameters.collect {
            case param if param.typeclass.encode.isObject =>
              val tc = param.typeclass
              val id = tc.encode.id
              tc.encode.definitions +                                 // nested object definitions
                Labelled(id, tc.encode.copy(definitions = Set.empty)) // nested object minus its old definitions
          }.flatten.toSet

        val required =
          caseClass.parameters.collect {
            case param if param.typeclass.encode.required =>
              param.label
          }

        JsonSchemaDocument(
          id = caseClass.typeName.full,
          schema = JsonSchema.Obj.Product(primitives ++ references, requiredKeys = required),
          title = getTitle(caseClass.annotations),
          description = getDescription(caseClass.annotations),
          definitions = definitions
        )
      }

    private def getTitle(in: Seq[Any]): Option[String] =
      in.collectFirst {
        case e if e.isInstanceOf[annotations.title] => e.asInstanceOf[annotations.title].text
      }

    private def getDescription(in: Seq[Any]): Option[String] =
      in.collectFirst {
        case e if e.isInstanceOf[annotations.description] => e.asInstanceOf[annotations.description].text
      }
  }

  def dispatch[T](sealedTrait: SealedTrait[Typeclass, T]): Typeclass[T] = new Typeclass[T] {
    def aggregatedCaseObjects: Seq[JsonSchemaDocument] =
      sealedTrait.subtypes
        .map(_.typeclass.encode.schema)
        .collect { case c @ JsonSchema.Obj.CaseObj(_) => c }
        .reduceOption(_ ++ _)
        .map(cObj => JsonSchemaDocument(id = sealedTrait.typeName.full, schema = cObj))
        .toSeq

    def nonCaseObjects: Seq[JsonSchemaDocument] =
      sealedTrait.subtypes
        .filterNot(_.typeclass.encode.isCaseObject)
        .map { term =>
          val tc = term.typeclass
          JsonSchemaDocument(
            id = term.typeName.full,
            schema = tc.encode.schema
          )
        }

    override def encode: JsonSchemaDocument = {
      val definitions =
        sealedTrait.subtypes.flatMap { term =>
          term.typeclass.encode.definitions
        }.toSet

      JsonSchemaDocument(
        id = sealedTrait.typeName.full,
        schema = JsonSchema.Obj.Sum(nonCaseObjects ++ aggregatedCaseObjects),
        definitions = definitions
      )
    }
  }

  implicit def gen[T]: Typeclass[T] = macro Magnolia.gen[T]
}
