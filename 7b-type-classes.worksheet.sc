// Type classess are a way to providing ad-hoc polymorphism
// Similary to extension method they do allow to provide an additional method of some type,
// however they can be limited to given context and be used to abstract over generic type

// imagine we would like to add JsonSerializable trait to selected types
// trait JsonSerializable { def toJson: String }
sealed trait JsonEncoder[T]:
  // abstract method that we need to implement in each JsonEncoder instance
  def encode(value: T): String
  // extension method that can be used on instance of objects having JsonEncoder defined in the scope
  extension (value: T) final def toJson: String = encode(value)

// Extension can be also defined outside the type class
// extension [T] (value: T) final def toJson(using encoder: JsonEncoder[T]): String = encoder.encode(value)

// We can define a type class for types we don't have control off, similary to extension methods
// It's a good practive to define type-classess for primitive types in type-class companion object
object JsonEncoder {
  given JsonEncoder[String] with
    override def encode(value: String): String =
      s""""${value}"""" // double-quote the string

  given JsonEncoder[Int] with
    override def encode(v: Int): String = v.toString
}
import JsonEncoder.given // Imports JsonEncoder for String and Int

case class Stats(count: Int, message: String) extends Base
object Stats {
  // For types we define instance of type class can be defined in companion object
  given JsonEncoder[Stats] with
    override def encode(value: Stats): String =
      s"""{"count": ${value.count.toJson}, "message": ${value.message.toJson}}"""
}
sealed trait Base
case class Result(module: String, stats: Stats) extends Base
object Result {
  given JsonEncoder[Result] = new JsonEncoder[Result] {
    override def encode(value: Result): String =
      s"""{"module": ${value.module.toJson}, "stats": ${value.stats.toJson}}"""
  }
}

// We can now use our JsonEncoder type classes to on our types
summon[JsonEncoder[String]].encode("Hello")
Result("module1", Stats(count = 10, message = "Tests passed")).toJson

// We do not define JsonEncoder[Long] so the following lines would not compile
// toListOfJsons(1L :: 2L :: Nil)
case class ClassWithoutEncoder(foo: Int)
// summon[JsonEncoder[ClassWithoutEncoder]]
// ClassWithoutEncoder(foo = 1).toJson

// By providing a type boundry we can abstract about argument of the method call
def toListOfJsons[T: JsonEncoder](elems: T*): List[String] =
  elems.map(_.toJson).to(List)

toListOfJsons(
  Result("module1", Stats(count = 10, message = "Hello world")),
  Result("module2", Stats(count = 12, message = "Hello Scala"))
)
toListOfJsons("1L", "2L")

// We cannot however mix different type classess in the arguments of a function call
// unless they have a common type which defined an instance of type class
val resultsAndStats: Seq[Base] = Seq(
  Result("module1", Stats(count = 10, message = "Hello world")),
  Stats(count = 12, message = "Hello Scala")
)
// toListOfJsons(resultsAndStats*)
def localEncoders() = {
  given JsonEncoder[Base] with
    override def encode(value: Base): String = value match {
      case v: Result => v.toJson
      case v: Stats  => v.toJson
    }
  // In this scope there is an instance of JsonEncoder[Base] so it would compile
  toListOfJsons(resultsAndStats*)
}
// toListOfJsons("hello", 123)

// Type classess can be also automatically derived for enums, case classes/objects and sealed types,
// however we would not discuss this feature today, example of usage:
// case class FooBar(foo: Int, bar: String) derives JsonEncoder
// https://dotty.epfl.ch/docs/reference/contextual/derivation.html
