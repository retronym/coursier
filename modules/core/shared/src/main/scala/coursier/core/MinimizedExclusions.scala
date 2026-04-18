package coursier.core

import coursier.core.Exclusions.{allNames, allOrganizations}
import dataclass.data

import scala.collection.mutable.ArrayBuffer

/** This file defines a special-purpose structure for exclusions that has the following
  * properties/goals:
  *   - The exclusion data is always minimized (minimized meaning overlapping rules are removed)
  *   - The data structure is split into various cases, optimizing common cases for join/meet
  *   - The hashcode is cached, such that recalculating the hashcode for these exclusions is cached.
  */
object MinimizedExclusions {

  val zero = MinimizedExclusions(ExcludeNone)
  val one  = MinimizedExclusions(ExcludeAll)

  sealed abstract class ExclusionData extends Product with Serializable {
    def apply(org: Organization, module: ModuleName): Boolean

    def join(other: ExclusionData): ExclusionData
    def meet(other: ExclusionData): ExclusionData

    def partitioned()
      : (Boolean, Set[Organization], Set[ModuleName], Set[(Organization, ModuleName)])
    def map(f: String => String): ExclusionData

    def size(): Int

    def subsetOf(other: ExclusionData): Boolean

    def toSet(): Set[(Organization, ModuleName)]

    def hasProperties: Boolean
  }

  case object ExcludeNone extends ExclusionData {
    override def apply(org: Organization, module: ModuleName): Boolean = true

    override def join(other: ExclusionData): ExclusionData = other
    override def meet(other: ExclusionData): ExclusionData = ExcludeNone
    override def partitioned()
      : (Boolean, Set[Organization], Set[ModuleName], Set[(Organization, ModuleName)]) =
      (false, Set.empty, Set.empty, Set.empty)
    override def map(f: String => String): ExclusionData = ExcludeNone

    override def size(): Int                              = 0
    override def subsetOf(other: ExclusionData): Boolean  = true
    override def toSet(): Set[(Organization, ModuleName)] = Set.empty

    def hasProperties: Boolean = false
  }

  case object ExcludeAll extends ExclusionData {
    override def apply(org: Organization, module: ModuleName): Boolean = false

    override def join(other: ExclusionData): ExclusionData = ExcludeAll
    override def meet(other: ExclusionData): ExclusionData = other

    override def partitioned()
      : (Boolean, Set[Organization], Set[ModuleName], Set[(Organization, ModuleName)]) =
      (true, Set.empty, Set.empty, Set.empty)
    override def map(f: String => String): ExclusionData = ExcludeAll

    override def size(): Int                              = 1
    override def subsetOf(other: ExclusionData): Boolean  = other == ExcludeAll
    private val _toSet: Set[(Organization, ModuleName)] = Set((allOrganizations, allNames))
    override def toSet(): Set[(Organization, ModuleName)] = _toSet

    def hasProperties: Boolean = false
  }

  @data class ExcludeSpecific(
    byOrg: Set[Organization],
    byModule: Set[ModuleName],
    specific: Set[(Organization, ModuleName)]
  ) extends ExclusionData {

    override def apply(org: Organization, module: ModuleName): Boolean =
      !byModule(module) &&
      !byOrg(org) &&
      !specific((org, module))

    override def join(other: ExclusionData): ExclusionData =
      other match {
        case ExcludeNone => this
        case ExcludeAll  => ExcludeAll
        case other: ExcludeSpecific =>
          val joinedByOrg    = byOrg ++ other.byOrg
          val joinedByModule = byModule ++ other.byModule
          def fullJoin = specific ++ other.specific
          def filteredJoin = scala.collection.immutable.HashSet.from(
            specific.iterator.filter { case e @ (org, module) =>
              !other.byOrg(org) && !other.byModule(module)
            } ++
              other.specific.iterator.filter { case e @ (org, module) =>
                !byOrg(org) && !byModule(module)
              })

          val joinedSpecific =
            if (joinedByOrg.isEmpty && joinedByModule.isEmpty) fullJoin
            else filteredJoin

          ExcludeSpecific(joinedByOrg, joinedByModule, joinedSpecific)
      }

    override def meet(other: ExclusionData): ExclusionData =
      other match {
        case ExcludeNone => this
        case ExcludeAll  => ExcludeAll
        case other: ExcludeSpecific =>
          val metByOrg    = byOrg intersect other.byOrg
          val metByModule = byModule intersect other.byModule

          val metSpecific =
            specific.filter { case e @ (org, module) =>
              other.byOrg(org) || other.byModule(module) || other.specific(e)
            } ++
              other.specific.filter { case e @ (org, module) =>
                byOrg(org) || byModule(module) || specific(e)
              }

          if (metByOrg.isEmpty && metByModule.isEmpty && metSpecific.isEmpty)
            ExcludeNone
          else
            ExcludeSpecific(metByOrg, metByModule, metSpecific)
      }

    override def partitioned()
      : (Boolean, Set[Organization], Set[ModuleName], Set[(Organization, ModuleName)]) =
      (false, byOrg, byModule, specific)

    override def map(f: String => String): ExclusionData = {
      def mapSetSkewedToNoOp[T](s: Set[T])(f: T => T): Set[T] = {
        val buffer = new ArrayBuffer[T](s.size)
        var changed = false
        for (elem <- s) {
          val t = f(elem)
          if (t != elem) changed = true
          buffer += t
        }
        if (changed) Set.from(buffer) else s
      }

      val newByOrg = mapSetSkewedToNoOp(byOrg)(_.map(f))
      val newByModule = mapSetSkewedToNoOp(byModule)(_.map(f))
      val newSpecific = mapSetSkewedToNoOp(specific) { case kv@(org, module) =>
        val newOrg = org.map(f)
        val newModule = module.map(f)
        if (newOrg == org && newModule == module) kv
        else (org.map(f), module.map(f))
      }
      if ((newByOrg eq byOrg) && (newByModule eq byModule) && (newSpecific eq specific)) this
      else new ExcludeSpecific(newByOrg, newByModule, newSpecific)
    }

    override def size(): Int = byOrg.size + byModule.size + specific.size

    override def subsetOf(other: ExclusionData): Boolean =
      other match {
        case ExcludeNone => false
        case ExcludeAll  => false
        case other: ExcludeSpecific =>
          byOrg.subsetOf(other.byOrg) &&
          byModule.subsetOf(other.byModule) &&
          specific.subsetOf(other.specific)
      }

    private lazy val _toSet = {
      val b = Set.newBuilder[(Organization, ModuleName)]
      byOrg.foreach(org => b.addOne((org, allNames)))
      byModule.foreach(name => b.addOne((allOrganizations, name)))
      b.addAll(specific)
      b.result()
    }

    override def toSet(): Set[(Organization, ModuleName)] = _toSet

    lazy val hasProperties: Boolean =
      byOrg.exists(_.value.contains("$")) ||
      byModule.exists(_.value.contains("$")) ||
      specific.exists(t => t._1.value.contains("$") || t._2.value.contains("$"))
  }

  def apply(exclusions: Set[(Organization, ModuleName)]): MinimizedExclusions =
    if (exclusions.isEmpty)
      zero
    else {
      val excludeByOrg0  = Set.newBuilder[Organization]
      val excludeByName0 = Set.newBuilder[ModuleName]
      var remaining0     = exclusions

      val it    = exclusions.iterator
      var isOne = false
      while (it.hasNext && !isOne) {
        val excl = it.next()
        if (excl._1 == allOrganizations) {
          if (excl._2 == allNames)
            isOne = true
          else
            excludeByName0 += excl._2
          remaining0 -= excl
        } else if (excl._2 == allNames) {
          excludeByOrg0 += excl._1
          remaining0 -= excl
        }
      }

      if (isOne)
        one
      else {

        val byOrg = excludeByOrg0.result()
        val byName = excludeByName0.result()
        val specific = remaining0
        MinimizedExclusions(ExcludeSpecific(
          byOrg,
          byName,
          specific
        ))
      }
    }
}

@data class MinimizedExclusions(data: MinimizedExclusions.ExclusionData) {
  def apply(org: Organization, module: ModuleName): Boolean = data(org, module)

  def join(other: MinimizedExclusions): MinimizedExclusions = {
    val newData = data.join(other.data)
    // If no data was changed, no need to construct a new instance and create a new hashcode
    if (newData eq this.data)
      this
    else if (newData eq other.data)
      other
    else
      MinimizedExclusions(newData)
  }

  def meet(other: MinimizedExclusions): MinimizedExclusions = {
    val newData = data.meet(other.data)
    // If no data was changed, no need to construct a new instance and create a new hashcode
    if (newData eq this.data)
      this
    else if (newData eq other.data)
      other
    else
      MinimizedExclusions(newData)
  }

  def map(f: String => String): MinimizedExclusions = {
    val newData = data.map(f)
    // If no data was changed, no need to construct a new instance and create a new hashcode
    if (newData eq this.data)
      this
    else
      MinimizedExclusions(newData)
  }

  def partitioned()
    : (Boolean, Set[Organization], Set[ModuleName], Set[(Organization, ModuleName)]) =
    data.partitioned()

  def isEmpty: Boolean = data == MinimizedExclusions.ExcludeNone

  def nonEmpty: Boolean = data != MinimizedExclusions.ExcludeNone

  def size(): Int = data.size()

  def subsetOf(other: MinimizedExclusions): Boolean = data.subsetOf(other.data)

  def toSet(): Set[(Organization, ModuleName)] = data.toSet()

  def toVector(): Vector[(Organization, ModuleName)] = data.toSet().toVector

  def toSeq(): Seq[(Organization, ModuleName)] = data.toSet().toSeq

  final override lazy val hashCode = data.hashCode()

  def hasProperties: Boolean =
    data.hasProperties
}
