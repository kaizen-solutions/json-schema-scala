package io.kaizensolutions.jsonschema

import io.kaizensolutions.jsonschema.annotations.JsonConstraints._
import magnolia1._

trait JsonSchemaEncoderMagnoliaDerivation extends Derivation[JsonSchemaEncoder] {
  override def join[T](caseClass: CaseClass[JsonSchemaEncoder, T]): JsonSchemaEncoder[T] =
    new JsonSchemaEncoder[T]:
      override def encode: JsonSchemaDocument =
        if (caseClass.isObject) {
          JsonSchemaDocument(
            id = caseClass.typeInfo.full,
            schema = JsonSchema.Obj.CaseObj(
              Set(caseClass.typeInfo.short)
            )
          )
        } else {
          val primitives: Seq[Labelled] =
            caseClass.params.collect {
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
            caseClass.params.collect {
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
            caseClass.params.collect {
              case param if param.typeclass.encode.isObject =>
                val tc = param.typeclass
                val id = tc.encode.id
                tc.encode.definitions +                                        // nested object definitions
                  Labelled(name = id, document = tc.encode.withoutDefinitions) // nested object minus its old definitions
            }.flatten.toSet

          val required =
            caseClass.params.collect {
              case param if param.typeclass.encode.required =>
                param.label
            }

          JsonSchemaDocument(
            id = caseClass.typeInfo.full,
            schema = JsonSchema.Obj.Product(caseClass.typeInfo.short, primitives ++ references, requiredKeys = required),
            title = getTitle(caseClass.annotations),
            description = getDescription(caseClass.annotations),
            definitions = definitions
          )
        }

  override def split[T](sealedTrait: SealedTrait[JsonSchemaEncoder, T]): JsonSchemaEncoder[T] = 
    new JsonSchemaEncoder[T]:
      override def encode: JsonSchemaDocument = 
        val caseObjectsSchemaDoc: Seq[JsonSchemaDocument] =
          aggregatedCaseObjects

        val (
          nonCaseObjectsSchemaDoc: Seq[JsonSchemaDocument],
          nonCaseObjectsDefinitions: Set[Labelled]
          ) =
          nonCaseObjects.foldLeft((Seq.empty[JsonSchemaDocument], Set.empty[Labelled])) {
            case ((accSchema, accDefs), (schema, defs)) =>
              (schema +: accSchema, defs ++ accDefs)
          }

        JsonSchemaDocument(
          id = sealedTrait.typeInfo.full,
          schema = JsonSchema.Obj.Sum(
            typeName = sealedTrait.typeInfo.short,
            terms = nonCaseObjectsSchemaDoc ++ caseObjectsSchemaDoc
          ),
          definitions = nonCaseObjectsDefinitions
        )
      

      private def aggregatedCaseObjects: Seq[JsonSchemaDocument] =
        sealedTrait.subtypes
          .map(_.typeclass.encode.schema)
          .collect { case c @ JsonSchema.Obj.CaseObj(_, _) => c }
          .reduceOption(_ ++ _)
          .map(cObj =>
            JsonSchemaDocument(
              id = sealedTrait.typeInfo.full,
              schema = cObj.copy(belongsToSum = Some(sealedTrait.typeInfo.full))
            )
          )
          .toSeq

      private def nonCaseObjects: Seq[(JsonSchemaDocument, Set[Labelled])] =
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
                .belongsTo(sealedTrait.typeInfo.full)
                .withoutDefinitions
            )
            (doc, definitions)
          }
}
