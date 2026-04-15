package coursier.core

import scala.collection.{AbstractSeq, Seq => CSeq}
import scala.collection.immutable.{Seq => ISeq}
import scala.collection.mutable

private[coursier] sealed abstract class LazyProperties
    extends AbstractSeq[(String, String)]
    with ISeq[(String, String)] {

  protected def lazyMap: scala.collection.immutable.Map[String, String]

  override def toMap[K, V](
    implicit ev: ((String, String)) <:< (K, V)
  ): scala.collection.immutable.Map[K, V] =
    lazyMap.asInstanceOf[scala.collection.immutable.Map[K, V]]
}

private[coursier] object LazyProperties {

  private case object EmptyProperties extends LazyProperties {
    override def iterator: Iterator[(String, String)] = Iterator.empty
    override def length: Int                          = 0
    override def apply(idx: Int): (String, String) =
      throw new IndexOutOfBoundsException(idx.toString)

    override protected val lazyMap: scala.collection.immutable.Map[String, String] =
      scala.collection.immutable.Map.empty
  }

  def concat(
    properties: CSeq[(String, String)],
    extraProperties: CSeq[(String, String)]
  ): ISeq[(String, String)] =
    concat(Seq(properties, extraProperties))

  def concatLayerMaps(
    layers: CSeq[scala.collection.Map[String, _]]
  ): ISeq[(String, String)] =
    concat(layers.iterator.map(mapLayerToSeq).toVector)

  def concat(
    layers: CSeq[CSeq[(String, String)]]
  ): ISeq[(String, String)] = {
    val normalizedLayers = layers.iterator.flatMap(extractLayers).toArray
    if (normalizedLayers.isEmpty) EmptyProperties
    else new ConcatProperties(normalizedLayers)
  }

  def filterKeysNotIn(
    properties: CSeq[(String, String)],
    excludedKeys: Set[String]
  ): ISeq[(String, String)] = {
    val normalizedLayers = extractLayers(properties).toArray
    if (normalizedLayers.isEmpty) EmptyProperties
    else if (excludedKeys.isEmpty) new ConcatProperties(normalizedLayers)
    else new FilteredProperties(normalizedLayers, excludedKeys)
  }

  def substitute(
    s: String,
    lookup: Map[String, String],
    trim: Boolean = false
  ): String = {
    val props = parse(s)
    substitute(s, lookup, trim, props)
  }

  def substitute(s: String, lookup: Map[String, String], trim: Boolean, props: ParsedSubstituteProps): String = {
    props match {
      case NoSubstitutions =>
        s
      case ParsedSubstitutions(parts) =>
        renderSubstitutions(s, parts, lookup, trim)
    }
  }

  sealed abstract class ParsedPropertyPart
  final case class PropertyLiteral(value: String) extends ParsedPropertyPart
  final case class PropertyReference(name: String) extends ParsedPropertyPart

  sealed abstract class ParsedSubstituteProps
  case object NoSubstitutions extends ParsedSubstituteProps
  final case class ParsedSubstitutions(
    parts: Array[ParsedPropertyPart]
  ) extends ParsedSubstituteProps

  private final class Entry(val value: String) {
    lazy val parsed: ParsedSubstituteProps =
      parse(value)
  }

  private final class Layer(val values: ISeq[(String, String)]) {
    lazy val index: java.util.Map[String, Entry] = {
      val m = new java.util.HashMap[String, Entry](math.max(16, values.length))
      var i = 0
      while (i < values.length) {
        val key = values(i)._1
        m.put(key, new Entry(values(i)._2))
        i += 1
      }
      m
    }
  }

  private def layerFromSeq(layer: CSeq[(String, String)]): Layer = {
    val normalized = layer match {
      case immutableLayer: ISeq[(String, String)] => immutableLayer.toVector
      case _                                       => layer.toVector
    }

    new Layer(normalized)
  }

  private def extractLayers(properties: CSeq[(String, String)]): Iterator[Layer] =
    properties match {
      case concat: ConcatProperties     => concat.layers.iterator
      case filtered: FilteredProperties => filtered.layers.iterator
      case other if other.isEmpty       => Iterator.empty
      case other                        => Iterator.single(layerFromSeq(other))
    }

  private def mapLayerToSeq(layer: scala.collection.Map[String, _]): ISeq[(String, String)] = {
    val b = Vector.newBuilder[(String, String)]

    layer.iterator.foreach {
      case (k, s: String) =>
        b += ((k, s))
      case (k, values: CSeq[_]) =>
        values.foreach {
          case s: String => b += ((k, s))
          case other =>
            throw new IllegalArgumentException(
              s"Property value sequences must contain only strings, got ${other.getClass.getName}"
            )
        }
      case (k, values: Iterable[_]) =>
        values.foreach {
          case s: String => b += ((k, s))
          case other =>
            throw new IllegalArgumentException(
              s"Property value collections must contain only strings, got ${other.getClass.getName}"
            )
        }
      case (_, other) =>
        throw new IllegalArgumentException(
          s"Property map values must be a String or a Seq[String], got ${other.getClass.getName}"
        )
    }

    b.result()
  }

  private def renderSubstitutions(
    raw: String,
    parts: Array[ParsedPropertyPart],
    lookup: Map[String, String],
    trim: Boolean
  ): String = {
    val b = new java.lang.StringBuilder(raw.length + 32)
    var substed = false
    var idx = 0
    while (idx < parts.length) {
      parts(idx) match {
        case PropertyLiteral(value) =>
          b.append(value)
        case PropertyReference(name) =>
          lookup.getOrElse(name, null) match {
            case null =>
              b.append("${")
              b.append(name)
              b.append('}')
            case v =>
              substed = true
              val v0 = if (trim) v.trim else v
              b.append(v0)
          }
      }
      idx += 1
    }
    if (substed) b.toString
    else raw
  }

  def fastApply(value: String, parsedSubstituteProps: ParsedSubstituteProps, f: (String => String)): String = f match {
    case s: Substitution => s.fasterApply(value, parsedSubstituteProps)
    case _ => f(value)
  }
  final class Substitution(lookup: Map[String, String], trim: Boolean) extends (String => String) {
    override def apply(v1: String): String = fasterApply(v1, parseSubstituteProps(v1))

    def fasterApply(s: String, parsedSubstituteProps: ParsedSubstituteProps): String = {
      substitute(s, lookup, trim, parsedSubstituteProps)
    }
  }

  def parse(s: String): ParsedSubstituteProps = {
    if (s.indexOf('$') < 0)
      NoSubstitutions
    else parseSubstituteProps(s)
  }

  private def parseSubstituteProps(s: String): ParsedSubstituteProps = {
    val parts         = new mutable.ArrayBuffer[ParsedPropertyPart]
    var hasProperties = false
    var idx           = 0

    while (idx < s.length) {
      var dolIdx = idx
      while (dolIdx < s.length && s.charAt(dolIdx) != '$')
        dolIdx += 1

      if (dolIdx > idx)
        appendLiteral(parts, s.substring(idx, dolIdx))

      if (dolIdx >= s.length)
        idx = dolIdx
      else if (dolIdx < s.length - 2 && s.charAt(dolIdx + 1) == '{') {
        var endIdx = dolIdx + 2
        while (endIdx < s.length && s.charAt(endIdx) != '}')
          endIdx += 1

        if (endIdx < s.length) {
          hasProperties = true
          parts += PropertyReference(s.substring(dolIdx + 2, endIdx))
          idx = endIdx + 1
        }
        else {
          appendLiteral(parts, "$")
          idx = dolIdx + 1
        }
      }
      else {
        appendLiteral(parts, "$")
        idx = dolIdx + 1
      }
    }

    if (!hasProperties)
      NoSubstitutions
    else
      ParsedSubstitutions(parts.toArray)
  }

  private def appendLiteral(
    parts: mutable.ArrayBuffer[ParsedPropertyPart],
    value: String
  ): Unit =
    if (value.nonEmpty)
      if (parts.nonEmpty)
        parts.last match {
          case PropertyLiteral(existing) =>
            parts(parts.length - 1) = PropertyLiteral(existing + value)
          case _: PropertyReference =>
            parts += PropertyLiteral(value)
        }
      else
        parts += PropertyLiteral(value)

  private abstract class LayeredLazyProperties(
    val layers: Array[Layer]
  ) extends LazyProperties {

    protected final lazy val layerSizes: Array[Int] =
      layers.map(_.values.length)

    protected final lazy val layerOffsets: Array[Int] = {
      val offsets = new Array[Int](layers.length)
      var i       = 0
      var offset  = 0
      while (i < layers.length) {
        offsets(i) = offset
        offset += layerSizes(i)
        i += 1
      }
      offsets
    }

    override def length: Int = {
      var total = 0
      var i     = 0
      while (i < layerSizes.length) {
        total += layerSizes(i)
        i += 1
      }
      total
    }

    final def pairAtGlobalIndex(idx: Int): (String, String) = {
      if (idx < 0)
        throw new IndexOutOfBoundsException(idx.toString)

      var remaining = idx
      var layerIdx  = 0
      while (layerIdx < layers.length) {
        val layerSize = layerSizes(layerIdx)
        if (remaining < layerSize)
          return layers(layerIdx).values(remaining)
        remaining -= layerSize
        layerIdx += 1
      }

      throw new IndexOutOfBoundsException(idx.toString)
    }

    final def winnerEntryForKeyOrNull(key: String): Entry = {
      var layerIdx = layers.length - 1
      while (layerIdx >= 0) {
        val entry = layers(layerIdx).index.get(key)
        if (entry != null)
          return entry
        layerIdx -= 1
      }
      null
    }
  }

  private final class LazyMapWithSubstitutedValues(
    owner: LayeredLazyProperties,
    includeKey: String => Boolean
  ) extends scala.collection.immutable.AbstractMap[String, String] {

    private[this] lazy val entries0 = {
      val b    = Vector.newBuilder[(String, String)]
      val seen = mutable.HashSet.empty[String]
      var layerIdx = owner.layers.length - 1

      while (layerIdx >= 0) {
        val layer = owner.layers(layerIdx)
        var idx   = layer.values.length - 1

        while (idx >= 0) {
          val (k, v) = layer.values(idx)
          if (includeKey(k) && !seen(k)) {
            seen += k
            b += ((k, v))
          }
          idx -= 1
        }

        layerIdx -= 1
      }

      b.result().reverse
    }

    override def getOrElse[V1 >: String](key: String, default: => V1): V1 = {
      if (!includeKey(key)) default
      else {
        owner.winnerEntryForKeyOrNull(key) match {
          case null => default
          case entry =>
            substitute(entry.value, this, false, entry.parsed)
        }
      }
    }

    override def get(key: String): Option[String] =
      if (!includeKey(key)) None
      else {
        owner.winnerEntryForKeyOrNull(key) match {
          case null => None
          case entry =>
            Some(substitute(entry.value, this, false, entry.parsed))
        }
      }

    override def iterator: Iterator[(String, String)] =
      entries0.iterator

    override def removed(key: String): scala.collection.immutable.Map[String, String] =
      scala.collection.immutable.Map.from(entries0).removed(key)

    override def updated[V1 >: String](
      key: String,
      value: V1
    ): scala.collection.immutable.Map[String, V1] =
      scala.collection.immutable.Map.from(entries0.map { case (k, v) => (k, v: V1) }).updated(key, value)
  }

  private final class ConcatProperties(
    layers: Array[Layer]
  ) extends LayeredLazyProperties(layers) {

    override def iterator: Iterator[(String, String)] =
      layers.iterator.flatMap(_.values.iterator)

    override def apply(idx: Int): (String, String) =
      pairAtGlobalIndex(idx)

    override protected lazy val lazyMap: scala.collection.immutable.Map[String, String] =
      new LazyMapWithSubstitutedValues(this, _ => true)
  }

  private final class FilteredProperties(
    layers: Array[Layer],
    excludedKeys: Set[String]
  ) extends LayeredLazyProperties(layers) {

    private[this] lazy val filteredLength = {
      var count    = 0
      var layerIdx = 0
      while (layerIdx < layers.length) {
        val layer = layers(layerIdx)
        var idx   = 0
        while (idx < layer.values.length) {
          if (!excludedKeys.contains(layer.values(idx)._1))
            count += 1
          idx += 1
        }
        layerIdx += 1
      }
      count
    }

    override def iterator: Iterator[(String, String)] =
      layers.iterator.flatMap(_.values.iterator).filter { case (k, _) => !excludedKeys.contains(k) }

    override def length: Int =
      filteredLength

    override def apply(idx: Int): (String, String) = {
      if (idx < 0)
        throw new IndexOutOfBoundsException(idx.toString)

      var remaining = idx
      var layerIdx  = 0
      while (layerIdx < layers.length) {
        val layer = layers(layerIdx)
        var i     = 0
        while (i < layer.values.length) {
          val kv = layer.values(i)
          if (!excludedKeys.contains(kv._1)) {
            if (remaining == 0)
              return kv
            remaining -= 1
          }
          i += 1
        }
        layerIdx += 1
      }

      throw new IndexOutOfBoundsException(idx.toString)
    }

    override protected lazy val lazyMap: scala.collection.immutable.Map[String, String] =
      new LazyMapWithSubstitutedValues(this, k => !excludedKeys.contains(k))
  }
}
