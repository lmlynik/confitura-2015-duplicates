package pl.mlynik

import org.scalatest.{Matchers, FunSuite}


class DeduplicatorTest extends FunSuite with Matchers {
  test("deduplicates simple integers") {
    val integers = Array(1, 1, 1, 1, 1, 2, 3, 4, 4, 5, 5, 6)
    Deduplicator(integers).toList should contain theSameElementsAs Seq(1, 2, 3, 4, 5, 6)
  }

  test("deduplicates simple classes using overridden hashcode and equals") {

    class Person(val name: String, val lastname: String) {
      override def hashCode(): Int = name.hashCode + lastname.hashCode

      override def toString: String = name + " " + lastname

      override def equals(person: Any): Boolean = this.hashCode == person.hashCode
    }

    val people = Array(
      new Person("john", "smith"),
      new Person("john", "smith"),
      new Person("john", "smith"),
      new Person("gemma", "bain"),
      new Person("gemma", "bain"),
      new Person("ashford", "rodges"),
      new Person("ashford", "rodges")
    )

    val deduplicated = Deduplicator(people).toList

    deduplicated should contain(new Person("john", "smith"))
  }

  test("deduplicates simple classes using provided Ordering") {

    class Person(val name: String, val lastname: String)

    def personHash(person: Person) = person.name.hashCode + person.lastname.hashCode

    val people = Array(
      new Person("john", "smith"),
      new Person("john", "smith"),
      new Person("john", "smith"),
      new Person("gemma", "bain"),
      new Person("gemma", "bain"),
      new Person("ashford", "rodges"),
      new Person("ashford", "rodges")
    )

    val unique = Deduplicator(people)(Ordering.by[Person, Int](p => p.name.hashCode + p.lastname.hashCode)).toList
    unique.size should equal(3)
    unique.map(personHash).toSet.size should equal(3) //we trust in scala set to reduce duplicate hashes for us
  }

  test("deduplicates simple case classes using provided hashocde") {

    case class Person(name: String, lastname: String)

    val people = Array(
      Person("john", "smith"),
      Person("john", "smith"),
      Person("john", "smith"),
      Person("gemma", "bain"),
      Person("gemma", "bain"),
      Person("ashford", "rodges"),
      Person("ashford", "rodges")
    )

    Deduplicator(people).toList should contain theSameElementsAs Seq(
      Person("john", "smith"),
      Person("gemma", "bain"),
      Person("ashford", "rodges")
    )
  }
}
