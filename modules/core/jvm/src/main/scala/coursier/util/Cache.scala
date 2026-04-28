package coursier.util

import java.util.concurrent.ConcurrentMap

import concurrentrefhashmap.ConcurrentReferenceHashMap

private[coursier] object Cache {
  private class IdentityKey {
  }
  private val caches = new ConcurrentReferenceHashMap[IdentityKey, ConcurrentMap[_, _]](
    8,
    ConcurrentReferenceHashMap.ReferenceType.WEAK,
    ConcurrentReferenceHashMap.ReferenceType.WEAK
  )
  def clearAll(): Unit = {
    caches.values().forEach(_.clear())
    caches.clear()
  }
  def createCache[T >: Null](): ConcurrentMap[T, T] = {
    val res = new ConcurrentReferenceHashMap[T, T](
      8,
      ConcurrentReferenceHashMap.ReferenceType.WEAK,
      ConcurrentReferenceHashMap.ReferenceType.WEAK
    )
    caches.put(new IdentityKey, res)
    res
  }

  def cacheMethod[T >: Null](instanceCache: ConcurrentMap[T, T])(t: T): T = {
    val first = instanceCache.get(t)
    if (first == null) {
      val previous = instanceCache.putIfAbsent(t, t)
      if (previous == null) t else previous
    }
    else
      first
  }
}
