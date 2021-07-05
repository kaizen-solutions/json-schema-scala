package io.kaizensolutions.jsonschema

import scala.annotation.StaticAnnotation

package object annotations {
  final case class comment(text: String)     extends StaticAnnotation
  final case class description(text: String) extends StaticAnnotation
  final case class title(text: String)       extends StaticAnnotation
}
