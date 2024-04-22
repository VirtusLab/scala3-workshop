// Basic types of collections are immutable
// They're defined in scala.collection and scala.collection.immutable
val array = Array('a', 'b', 'c')
val list = List(1, 2, 3)
val list2 = 1 :: 2 :: 3 :: Nil
val vector = Vector(1.0, 2.0, 3.0)
val set = Set("a", "b", "a", "c")
val map = Map(
  "Adam" -> "Nowak",
  ("John", "Smith")
)
val range = 0 until 20 // exclusive range 0..19
val rangeIn = 0 to 20 // inclusive range 0..20

// Elements of the LazyListy are evaluated lazily upon the first usage
val stream = LazyList.from(0)
// stream.take(5).toList

// If the concreate implementation of the collection does not matter for us, we use use default Seq and IndexesSeq aliases
val seq = Seq("hello", "world")
val iseq = IndexedSeq("hello", "world")

// Mutable variants of most of the collections are available in the scala.collection.mutable package
import scala.collection.mutable
val mList = mutable.ListBuffer("foo")
mList += "bar"
// mList -= "foo"

// Basic operations on the collections
// Filter
def isEven(v: Int) = v % 2 == 0
0.until(10).filter(isEven)
0.until(10).filterNot(isEven(_))

// Partition - creates two collections based on the predicate, left - containg the elements mathich the predicate, and right for elements that do not
val (even, uneven) = 0.until(100).partition(isEven)

// map - it's a Monoid, it's used to apply given function to each element if the collection
0.until(10)
  .map(_ * 12 / 3)
  .map(v => v * 2)

// flatMap - it's a Monad which applies given function and returns some container, in our case it returns a collections
0.until(10).flatMap(v => List(-v, v))
// flatten - used to replace collection of collections to a single collection
List(List("a"), List("b")).flatten

// collect - allows to apply a partial function on elements of the collection
// If some elements are not handled in the partial function they would not be contained in the results
0.until(20).collect {
  case i if i % 3 == 0 & i % 5 == 0 => "foobar"
  case i if i % 3 == 0              => "foo"
  case i if i % 5 == 0              => "bar"
  case i                            => i
}

// find
0.until(10).find(_ == 5)

// pagination on the collections
val nums = 0
  .to(100)
  .toList
  .tail // Usuń pierwszy element
  .drop(10) // Usuń pierwsze 10 elementów
  .init // Usń ostatni element
  .take(5) // Weż 5 pierwszych elementów
  .reverse // Odwróc kolejność

nums.head
nums.headOption
nums.last
nums.lastOption

nums.indexWhere(_ == 13)
nums.lastIndexWhere(_ == 42)

// Reducing the collections
// Fold takes an initial value (state) and a function mapping the state and the current collection element returning a new state
val factorial = 1L.to(5L).fold(1L) { (acc, elem) => acc * elem }

// Reduce similiar to fold but does not take an initial value, instead take a value of the first element
val factorial2 = 1.to(5).reduce(_ * _)
// Similiar to fold but yield every intermiediate result
val factorailSeq = 1.to(5).scan(1)(_ * _)
// fold,reduce,scan have also an alternatives xLeft and xRight defining where the state should be put in the mapping function

val groupedByLastDigit = 0.until(100).groupBy(_.toString.last)
groupedByLastDigit.keySet.toList.sorted

groupedByLastDigit.map { (key, seq) =>
  Integer.valueOf(key.toString) -> seq.sum
}

// Option
// Option is a popular way of abstracting optional values
Option(null: String).map(_.length)
Option("msg").map(_.length)
