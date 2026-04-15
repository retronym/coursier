package coursier.benchmark

import java.util.concurrent.TimeUnit

import coursier.core.{Info, Overrides, Project, Resolution, Type}
import coursier.util.StringInterpolators._
import coursier.version.Version
import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Mode, OutputTimeUnit, Scope, State}

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class PropertiesTests {

  @Benchmark
  def substitutePlainString(state: PropertiesState): String =
    Resolution.substituteProps(state.plainString, state.lookup)

  @Benchmark
  def substituteInterpolatedString(state: PropertiesState): String =
    Resolution.substituteProps(state.interpolatedString, state.lookup)

  @Benchmark
  def materializeProjectProperties(state: PropertiesState): Int = {
    val properties = Resolution.projectProperties(state.project)
    var total      = 0

    val it = properties.iterator
    while (it.hasNext) {
      val (k, v) = it.next()
      total += k.length + v.length
    }

    total
  }
}

@State(Scope.Benchmark)
class PropertiesState {

  val lookup = Map(
    "scala.binary.version" -> "2.13",
    "base.version"         -> "1.0.0",
    "module.name"          -> "demo-core",
    "suffix"               -> "bin",
    "target.version"       -> "${base.version}-${suffix}"
  )

  val plainString = "io.get-coursier:coursier-cli:2.1.25"

  val interpolatedString =
    "${project.groupId}:${module.name}_${scala.binary.version}:${target.version}"

  val project = Project(
    mod"bench:lazy-properties",
    Version("1.0.0"),
    Nil,
    Map.empty,
    None,
    Nil,
    Seq(
      "scala.binary.version" -> "2.13",
      "base.version"         -> "1.0.0",
      "module.name"          -> "${project.artifactId}_${scala.binary.version}",
      "module.version"       -> "${base.version}",
      "target.version"       -> "${module.version}-bin",
      "final.name"           -> "${module.name}:${target.version}"
    ),
    Nil,
    None,
    None,
    Some(Type.jar),
    relocated = false,
    None,
    Nil,
    Info.empty,
    Overrides.empty,
    Map.empty,
    Map.empty
  )
}