package coursier.core

import coursier.core.LazyProperties.NoSubstitutions
import coursier.version.{VersionConstraint => VersionConstraint0, VersionInterval => VersionInterval0}
import dataclass.{data, since}

import scala.collection.mutable

object DependencyManagement {
  type Map        = scala.collection.immutable.Map[Key, Values]
  type GenericMap = scala.collection.Map[Key, Values]

  @data(cachedHashCode = true) class Key(
    organization: Organization,
    name: ModuleName,
    `type`: Type,
    classifier: Classifier
  ) {
    private lazy val orgHasProperties = organization.parsedValue ne NoSubstitutions
    private lazy val nameIndexOfDollar = name.value.indexOf('$')
    private lazy val typeIndexOfDollar = `type`.value.indexOf('$')
    private lazy val classifierIndexOfDollar = classifier.value.indexOf('$')
    val hasProperties = orgHasProperties || nameIndexOfDollar >= 0 || typeIndexOfDollar >= 0 || classifierIndexOfDollar >= 0

    def map(f: String => String): Key = {
      val newOrg        = organization.map(f)
      val newName       = name.map(f)
      val newType       = `type`.map(f)
      val newClassifier = classifier.map(f)
      if (
        organization != newOrg || name != newName || `type` != newType || classifier != newClassifier
      )
        Key(
          organization = newOrg,
          name = newName,
          `type` = newType,
          classifier = newClassifier
        )
      else
        this
    }

    def fakeModule: Module =
      Module(organization, name, Map.empty)

    // Mainly there for sorting purposes
    def repr: String =
      s"${organization.value}:${name.value}:${`type`.value}:${classifier.value}"
  }

  object Key {
    def from(dep: Dependency): Key =
      dep.depManagementKey
  }

  @data(cachedHashCode = true) class Values(
    config: Configuration,
    versionConstraint: VersionConstraint0,
    minimizedExclusions: MinimizedExclusions,
    optional: Boolean,
    @since("2.1.25")
    global: Boolean = false
  ) {

    @deprecated("Use the override accepting a VersionConstraint instead", "2.1.25")
    def this(
      config: Configuration,
      version: String,
      minimizedExclusions: MinimizedExclusions,
      optional: Boolean
    ) = this(
      config,
      VersionConstraint0(version),
      minimizedExclusions,
      optional
    )

    @deprecated("Use versionConstraint instead", "2.1.25")
    def version: String =
      versionConstraint.asString
    @deprecated("Use withVersionConstraint instead", "2.1.25")
    def withVersion(newVersion: String): Values =
      if (newVersion == version) this
      else withVersionConstraint(VersionConstraint0(newVersion))

    def isEmpty: Boolean =
      config.value.isEmpty && versionConstraint.asString.isEmpty && minimizedExclusions.isEmpty && !optional
    def fakeDependency(key: Key): Dependency =
      Dependency(
        key.fakeModule,
        versionConstraint,
        VariantSelector.ConfigurationBased(config),
        minimizedExclusions,
        Publication("", key.`type`, Extension.empty, key.classifier),
        optional = optional,
        transitive = true
      )
    def orElse(other: Values): Values = {
      val newConfig = if (config.value.isEmpty) other.config else config
      val newVersion =
        if (versionConstraint.asString.isEmpty) other.versionConstraint else versionConstraint
      val newExcl     = other.minimizedExclusions.join(minimizedExclusions)
      val newOptional = optional || other.optional
      if (
        config != newConfig || versionConstraint != newVersion || minimizedExclusions != newExcl || optional != newOptional
      )
        Values(
          newConfig,
          newVersion,
          newExcl,
          newOptional
        )
      else
        this
    }
    private val parsedConfig = LazyProperties.parse(config.value)
    def mapButVersion(f: String => String): Values = {
      val newConfigValue = LazyProperties.fastApply(config.value, parsedConfig, f)
      val newConfig = if (newConfigValue eq config.value) config else Configuration(newConfigValue)
      val newExcl   = minimizedExclusions.map(f)
      if ((config != newConfig) || (minimizedExclusions ne newExcl))
        Values(
          config = newConfig,
          versionConstraint = versionConstraint,
          minimizedExclusions = newExcl,
          // FIXME This might have been a string like "${some-prop}" initially :/
          optional = optional
        )
      else
        this
    }
    private lazy val parsedVersionConstraint = LazyProperties.parse(versionConstraint.asString)
    def mapVersion(f: String => String): Values = {
      val origVersionStr = versionConstraint.asString
      val newVersionStr = LazyProperties.fastApply(origVersionStr, parsedVersionConstraint, f)
      if (origVersionStr eq newVersionStr) this
      else withVersionConstraint(VersionConstraint0(newVersionStr))
    }
    val hasProperties =  config.value.contains("$") ||
      versionConstraint.asString.contains("$") ||
      minimizedExclusions.hasProperties
    override def toString(): String = {
      var fields = Seq(
        config.toString,
        versionConstraint.toString,
        minimizedExclusions.toString,
        optional.toString
      )
      if (global)
        fields = fields :+ global.toString
      fields.mkString("Values(", ", ", ")")
    }
  }

  object Values {
    val empty = Values(
      config = Configuration.empty,
      versionConstraint = VersionConstraint0.empty,
      minimizedExclusions = MinimizedExclusions.zero,
      optional = false
    )

    def from(config: Configuration, dep: Dependency): Values =
      Values(
        config,
        dep.versionConstraint,
        dep.minimizedExclusions,
        dep.optional
      )

    @deprecated("Use the override accepting a VersionConstraint instead", "2.1.25")
    def apply(
      config: Configuration,
      version: String,
      minimizedExclusions: MinimizedExclusions,
      optional: Boolean
    ): Values = apply(
      config,
      VersionConstraint0(version),
      minimizedExclusions,
      optional
    )
  }

  def entry(config: Configuration, dep: Dependency): (Key, Values) =
    (Key.from(dep), Values.from(config, dep))

  /** Converts a sequence of dependency management entries to a dependency management map
    *
    * The map having at most one value per key, rather than possibly several in the sequence
    *
    * This composes the values together, keeping the version of the first one, and adding their
    * exclusions if `composeValues` is true (the default). In particular, this respects the order of
    * values in the incoming sequence, and makes sure the values in the initial map go before those
    * of the sequence.
    */
  def add(
    initialMap: Map,
    entries: Seq[(Key, Values)],
    composeValues: Boolean = true
  ): Map =
    if (entries.isEmpty)
      initialMap
    else {
      val b = new mutable.HashMap[Key, Values]
      b.sizeHint(initialMap.size + entries.length)
      b ++= initialMap
      val it = entries.iterator
      while (it.hasNext) {
        val (key0, incomingValues) = it.next()
        val newValues = b.get(key0) match {
          case Some(previousValues) =>
            if (composeValues) previousValues.orElse(incomingValues)
            else previousValues
          case None =>
            incomingValues
        }
        b += ((key0, newValues))
      }
      b.result().toMap
    }

  import java.lang.invoke.{MethodHandles, MethodType}
  import scala.collection.mutable

  private val lookup = MethodHandles.lookup()

  private final val getOrElseMH =
    lookup.findVirtual(
      Class.forName("scala.collection.immutable.HashMapBuilder"),
      "getOrElse",
      MethodType.methodType(
        classOf[java.lang.Object],
        classOf[java.lang.Object],
        classOf[java.lang.Object]
      )
    )

  final case class CacheKey(
                             initial: Map,
                             entries: Seq[GenericMap],
                             composeValues: Boolean
                           ) {
    override lazy val hashCode = scala.util.hashing.MurmurHash3.productHash(this)
  }
  import java.util.concurrent.ConcurrentHashMap
  import java.util.concurrent.atomic.LongAdder
  object AddAllCache {
    private val cache =
      new ConcurrentHashMap[CacheKey, Map]()

    val hits   = new LongAdder()
    val misses = new LongAdder()

    def getOrCompute(key: CacheKey)(compute: => Map): Map = {
//      stats()
      val existing = cache.get(key)
      if (existing != null) {
        hits.increment()

        existing
      } else {
        misses.increment()
        val value = compute
        val prev = cache.putIfAbsent(key, value)
        if (prev != null) prev else value
      }
    }

    def stats(): Unit = {
      val h = hits.sum()
      val m = misses.sum()
      val total = h + m
      val hitRate =
        if (total == 0) 0.0
        else h.toDouble / total

      println(
        f"[AddAllCache] hits=$h%,d misses=$m%,d hitRate=${hitRate * 100}%.2f%% size=${cache.size()}"
      )
    }
    // 👇 shutdown hook
    println(">[AddAllCache] starting up...")
    Runtime.getRuntime.addShutdownHook(new Thread(() => {
     stats()
    }))

  }
  def addAll(
    initialMap: Map,
    entries: Seq[GenericMap],
    composeValues: Boolean = true
                  ): Map = {
    if (entries.forall(_.isEmpty)) initialMap
    else {
      val key = CacheKey(initialMap, entries, composeValues)

      AddAllCache.getOrCompute(key) {
        addAllUncached(initialMap, entries, composeValues)
      }
    }

  }

  def addAllUncached(
                      initialMap: Map,
                      entries: Seq[GenericMap],
                      composeValues: Boolean = true
                  ): Map = {
//    assert(initialMap.isEmpty)
//    println("addAllUncached: " + entries.size + " entries, entries.map(_.size) = " + entries.map(_.size) +  " " + initialMap.size + " initialMap, composeValues=" + composeValues + ")")
    val builder = scala.collection.immutable.HashMap.newBuilder[Key, Values]

    builder ++= initialMap

    val it = entries.iterator.flatMap(_.iterator)
    while (it.hasNext) {
      val (key, incoming) = it.next()

      // 🔥 reflective fast-path access into builder's internal map state
      val prev =
        getOrElseMH
          .invoke(builder, key.asInstanceOf[AnyRef], null)
          .asInstanceOf[Values]

      if (prev != null) {
        if (composeValues) {
          val composed = prev.orElse(incoming)
          if (composed != prev) {
            builder += (key -> composed)
          }
        }
      } else {
        builder += (key -> incoming)
      }
    }
    builder.result()
  }

  def addDependencies(
    map: Map,
    deps: Seq[(Configuration, Dependency)],
    composeValues: Boolean = true
  ): Map =
    add(
      map,
      deps.map {
        case (config, dep) =>
          entry(config, dep)
      },
      composeValues = composeValues
    )

}
