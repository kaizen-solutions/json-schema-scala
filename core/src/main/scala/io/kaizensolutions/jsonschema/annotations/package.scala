package io.kaizensolutions.jsonschema

import scala.annotation.StaticAnnotation

package object annotations {
  // General
  final case class comment(text: String)     extends StaticAnnotation
  final case class description(text: String) extends StaticAnnotation
  final case class title(text: String)       extends StaticAnnotation

  // Numeric
  final case class minimum(number: Double)    extends StaticAnnotation
  final case class maximum(number: Double)    extends StaticAnnotation
  final case class multipleOf(number: Double) extends StaticAnnotation
}
