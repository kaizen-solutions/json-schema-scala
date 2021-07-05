# JSON Schema for Scala 

This library describes JSON Schema documents in Scala and provides support for turning Scala data-types into JSON Schema 
documents automatically using Magnolia. This library does not attempt to implement all the features of JSON Schema 
(and currently only targets draft 7). 

## Usage 

You can build JSON Schema documents yourself from scratch, or you can use the JSON Schema Encoder typeclass which will 
automatically build a JSON Schema document from most Scala data-types. For example:

```scala
import io.kaizensolutions.jsonschema._

@title("A representation of a Person")
final case class Person(
  @description("age of the person in years") age: Option[Int],
  @description("full name of the person") name: String,
  device: Device
)

sealed trait Device
object Device {
  private final case class Simple(id: Int, reading: Reading)       extends Device
  private final case class Cluster(groupId: Int, reading: Reading) extends Device

  def cluster(groupId: Int, reading: Reading): Device = Cluster(groupId, reading)
  def simple(id: Int, reading: Reading): Device = Simple(id, reading)
}

final case class Reading(
  @description("poll frequency") pollFreq: Long,
  last: Option[Int]
)
```

You can automatically derive a JSON Schema document for a `Person` like so:
```scala
val jssDoc: JsonSchemaDocument = 
  JsonSchemaEncoder[Person].encode
```

`JsonSchemaDocument` is a Scala data-type that you can further manipulate if you like. You can also turn it into JSON
using one of the renderers (like `circe`):

```scala
import io.kaizensolutions.jsonschema.renderers.circe._

jssDoc.toJson.spaces2
```

This will generate the following JSON Schema document:

<details>
<summary>Click here to see the document rendered in JSON</summary>
<p>

```json
{
  "$schema" : "http://json-schema.org/draft-07/schema#",
  "$id" : "io.kaizensolutions.jsonschema.Person",
  "type" : "object",
  "properties" : {
    "age" : {
      "type" : "integer",
      "description" : "age of the person in years"
    },
    "name" : {
      "type" : "string",
      "description" : "full name of the person"
    },
    "device" : {
      "$ref" : "io.kaizensolutions.jsonschema.Device"
    }
  },
  "required" : [
    "name",
    "device"
  ],
  "additionalProperties" : true,
  "definitions" : {
    "io.kaizensolutions.jsonschema.Reading" : {
      "$id" : "io.kaizensolutions.jsonschema.Reading",
      "type" : "object",
      "properties" : {
        "pollFreq" : {
          "type" : "number",
          "description" : "poll frequency"
        },
        "last" : {
          "type" : "integer"
        }
      },
      "required" : [
        "pollFreq"
      ],
      "additionalProperties" : true
    },
    "io.kaizensolutions.jsonschema.Device" : {
      "$id" : "io.kaizensolutions.jsonschema.Device",
      "oneOf" : [
        {
          "type" : "object",
          "properties" : {
            "groupId" : {
              "type" : "integer"
            },
            "reading" : {
              "$ref" : "io.kaizensolutions.jsonschema.Reading"
            }
          },
          "required" : [
            "groupId",
            "reading"
          ],
          "additionalProperties" : true
        },
        {
          "type" : "object",
          "properties" : {
            "id" : {
              "type" : "integer"
            },
            "reading" : {
              "$ref" : "io.kaizensolutions.jsonschema.Reading"
            }
          },
          "required" : [
            "id",
            "reading"
          ],
          "additionalProperties" : true
        }
      ]
    }
  },
  "title" : "A representation of a Person"
}
```

</p>
</details>

## Inspiration

This library is heavily inspired by Andriy Onyshchuk's amazing [JSON Schema library](https://github.com/andyglow/scala-jsonschema) 
which is based on macros. This library takes a slightly different approach and uses Magnolia instead to do typeclass derivation in
the hopes of making the Scala 3 migration process a lot easier.
