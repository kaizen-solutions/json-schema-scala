package io.kaizensolutions.jsonschema.annotations

import scala.annotation.StaticAnnotation

object Annotations {
  // General
  final case class comment(text: String)     extends StaticAnnotation
  final case class description(text: String) extends StaticAnnotation
  final case class title(text: String)       extends StaticAnnotation

  // Numeric
  final case class minimum(number: Double)          extends StaticAnnotation
  final case class exclusiveMinimum(number: Double) extends StaticAnnotation
  final case class maximum(number: Double)          extends StaticAnnotation
  final case class exclusiveMaximum(number: Double) extends StaticAnnotation
  final case class multipleOf(number: Double)       extends StaticAnnotation

  // String
  final case class minimumLength(number: Int) extends StaticAnnotation
  final case class maximumLength(number: Int) extends StaticAnnotation
}
