object BuildSettings {
  val scala2Version = "2.13.6"

  val scalaCompilerOptions: Seq[String] = Seq(
    "-Vimplicits",
    "-Vtype-diffs"
  )
}
