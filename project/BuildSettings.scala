object BuildSettings {
  val scala2Version = "2.13.6"

  val scalaCompilerOptions: Seq[String] = Seq(
    "-Xfatal-warnings",
    "-Vimplicits",
    "-Vtype-diffs"
  )
}
