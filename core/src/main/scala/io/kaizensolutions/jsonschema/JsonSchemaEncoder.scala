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

  def simple[A](primType: JsonSchema.Primitive): JsonSchemaEncoder[A] =
    new JsonSchemaEncoder[A] {
      override def encode: JsonSchemaDocument =
        JsonSchemaDocument(
          schema = primType,
          id = primType.render
        )
    }

  implicit val nullSchemaEncoder: JsonSchemaEncoder[Null] =
    simple[Null](JsonSchema.Primitive.Null)

  implicit val booleanSchemaEncoder: JsonSchemaEncoder[Boolean] =
    simple[Boolean](JsonSchema.Primitive.Bool)

  implicit val stringJsonSchemaEncoder: JsonSchemaEncoder[String] =
    simple[String](JsonSchema.Primitive.Str(None, None))

  private def numericEncoder[A](numericType: JsonSchema.Primitive.NumericType): Typeclass[A] =
    simple[A](JsonSchema.Primitive.Numeric(numericType, None, None))

  implicit val intJsonSchemaEncoder: JsonSchemaEncoder[Int] =
    numericEncoder[Int](JsonSchema.Primitive.NumericType.Int)

  implicit val shortJsonSchemaEncoder: JsonSchemaEncoder[Short] =
    numericEncoder[Short](JsonSchema.Primitive.NumericType.Num)

  implicit val longJsonSchemaEncoder: JsonSchemaEncoder[Long] =
    numericEncoder[Long](JsonSchema.Primitive.NumericType.Num)

  implicit val doubleJsonSchemaEncoder: JsonSchemaEncoder[Double] =
    numericEncoder[Double](JsonSchema.Primitive.NumericType.Num)

  implicit val floatJsonSchemaEncoder: JsonSchemaEncoder[Float] =
    numericEncoder[Float](JsonSchema.Primitive.NumericType.Num)

  implicit val byteJsonSchemaEncoder: JsonSchemaEncoder[Byte] =
    numericEncoder[Byte](JsonSchema.Primitive.NumericType.Num)

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
          schema = JsonSchema.Obj.CaseObj(
            Set(caseClass.typeName.short)
          )
        )
      } else {
        val primitives: Seq[Labelled] =
          caseClass.parameters.collect {
            case param if param.typeclass.encode.isPrimitive =>
              val description     = getDescription(param.annotations)
              val title           = getTitle(param.annotations)
              val key             = param.label
              val tcInstance      = param.typeclass
              val withConstraints = applyConstraints(param.annotations, tcInstance.encode.schema)

              Labelled(
                name = key,
                document = tcInstance.encode.copy(
                  description = description,
                  title = title,
                  schema = withConstraints
                )
              )
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
              tc.encode.definitions +                                        // nested object definitions
                Labelled(name = id, document = tc.encode.withoutDefinitions) // nested object minus its old definitions
          }.flatten.toSet

        val required =
          caseClass.parameters.collect {
            case param if param.typeclass.encode.required =>
              param.label
          }

        JsonSchemaDocument(
          id = caseClass.typeName.full,
          schema = JsonSchema.Obj.Product(caseClass.typeName.short, primitives ++ references, requiredKeys = required),
          title = getTitle(caseClass.annotations),
          description = getDescription(caseClass.annotations),
          definitions = definitions
        )
      }

    private def applyConstraints(annotations: Seq[Any], schema: JsonSchema): JsonSchema =
      schema match {
        case num: JsonSchema.Primitive.Numeric =>
          num.copy(
            multipleOf = getMultipleOf(annotations),
            rangeConstraints = JsonSchema.Primitive
              .NumericRangeConstraints(
                minimum = getMinimum(annotations),
                exclusiveMinimum = getExclMinimum(annotations),
                maximum = getMaximum(annotations),
                exclusiveMaximum = getExclMaximum(annotations)
              )
              .normalize
          )

        case str: JsonSchema.Primitive.Str =>
          str.copy(lengthConstraint =
            JsonSchema.Primitive
              .StrLengthConstraint(
                min = getMinLength(annotations),
                max = getMaxLength(annotations)
              )
              .normalize
          )

        case ignore =>
          ignore
      }

    private def getMultipleOf(in: Seq[Any]): Option[Double] =
      in.collectFirst {
        case e if e.isInstanceOf[annotations.multipleOf] => e.asInstanceOf[annotations.multipleOf].number
      }

    private def getMinimum(in: Seq[Any]): Option[Double] =
      in.collectFirst {
        case e if e.isInstanceOf[annotations.minimum] => e.asInstanceOf[annotations.minimum].number
      }

    private def getMaximum(in: Seq[Any]): Option[Double] =
      in.collectFirst {
        case e if e.isInstanceOf[annotations.maximum] => e.asInstanceOf[annotations.maximum].number
      }

    private def getMinLength(in: Seq[Any]): Option[Int] =
      in.collectFirst {
        case e if e.isInstanceOf[annotations.minimumLength] => e.asInstanceOf[annotations.minimumLength].number
      }

    private def getMaxLength(in: Seq[Any]): Option[Int] =
      in.collectFirst {
        case e if e.isInstanceOf[annotations.maximumLength] => e.asInstanceOf[annotations.maximumLength].number
      }

    private def getExclMinimum(in: Seq[Any]): Option[Double] =
      in.collectFirst {
        case e if e.isInstanceOf[annotations.exclusiveMinimum] => e.asInstanceOf[annotations.exclusiveMinimum].number
      }

    private def getExclMaximum(in: Seq[Any]): Option[Double] =
      in.collectFirst {
        case e if e.isInstanceOf[annotations.exclusiveMaximum] => e.asInstanceOf[annotations.exclusiveMaximum].number
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
        .collect { case c @ JsonSchema.Obj.CaseObj(_, _) => c }
        .reduceOption(_ ++ _)
        .map(cObj =>
          JsonSchemaDocument(
            id = sealedTrait.typeName.full,
            schema = cObj.copy(belongsToSum = Some(sealedTrait.typeName.full))
          )
        )
        .toSeq

    def nonCaseObjects: Seq[(JsonSchemaDocument, Set[Labelled])] =
      sealedTrait.subtypes
        .filterNot(_.typeclass.encode.isCaseObject)
        .map { term =>
          // if this is not a case object then a term of a sum type has to be a case class
          // this means we need to turn them into refs and add their properties as definitions
          val orig = term.typeclass.encode
          val ref  = JsonSchema.Reference(orig.id)
          val doc  = JsonSchemaDocument(schema = ref, id = orig.id)
          val definitions = orig.definitions + Labelled(
            name = orig.id,
            document = orig
              .belongsTo(sealedTrait.typeName.full)
              .withoutDefinitions
          )
          (doc, definitions)
        }

    override def encode: JsonSchemaDocument = {
      val caseObjectsSchemaDoc: Seq[JsonSchemaDocument] =
        aggregatedCaseObjects

      val (
        nonCaseObjectsSchemaDoc: Seq[JsonSchemaDocument],
        nonCaseObjectsDefinitions: Set[Labelled]
      ) = {
        nonCaseObjects.foldLeft((Seq.empty[JsonSchemaDocument], Set.empty[Labelled])) {
          case ((accSchema, accDefs), (schema, defs)) =>
            (schema +: accSchema, defs ++ accDefs)
        }
      }

      JsonSchemaDocument(
        id = sealedTrait.typeName.full,
        schema = JsonSchema.Obj.Sum(
          typeName = sealedTrait.typeName.short,
          terms = nonCaseObjectsSchemaDoc ++ caseObjectsSchemaDoc
        ),
        definitions = nonCaseObjectsDefinitions
      )
    }
  }

  implicit def gen[T]: Typeclass[T] = macro Magnolia.gen[T]
}
