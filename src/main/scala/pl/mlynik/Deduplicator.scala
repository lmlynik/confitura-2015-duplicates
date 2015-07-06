package pl.mlynik

//O(2n)
//drawback in form of loosing the ordering
object Deduplicator {
  def apply[T](collection: Array[T])(implicit ordering: Ordering[T] = Ordering.by[T, Int](_.hashCode())): Iterable[T] = {
    util.Sorting.quickSort(collection)

    collection.foldRight(List.empty[T]) {
      case (item, aggregate) =>
        if (aggregate.nonEmpty && ordering.compare(aggregate.head, item) == 0)
          aggregate
        else
          item :: aggregate
    }
  }
}
