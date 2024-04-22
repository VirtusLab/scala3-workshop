// https://docs.scala-lang.org/tour/pattern-matching.html

sealed abstract class Person
case class Employee(name: String, age: Int, active: Boolean) extends Person
case class Customer(name: String) extends Person
class UnkownPerson extends Person()

val p: Person = Customer("vl")
p match {
  case Employee(name, age, false)      => "not active"
  case Employee(_, age, _) if age < 22 => "junior"
  case emp @ Employee(name, _, _)      => s"employee $emp"
  case Customer(_)                     => "customer"
  case p: Person                       => s"other person $p"
}

val ScalaVersion = raw"(\d)\.(\d+)\.(\d+)(-.*)?$$".r
val ScalaVersion(major, minor, patch, _) = "3.1.1-RC1": @unchecked

"3.1.1" match {
  case ScalaVersion(major, minor, _, PreRelease(preV)) =>
    s"pre-release: $major.$minor - $preV"
  case ScalaVersion(IsScala3(), _, _, _) => "Scala 3 release"
  case s"2.${minor}.${patch}"            => s"Scala 2.$minor release"
  case Parts(major, minor, other @ _*) =>
    s"other sem ver version $major.$minor - ${other.mkString("/")}"
  case other => s"other version $other"
}

// https://docs.scala-lang.org/tour/extractor-objects.html
object PreRelease {
  def unapply(s: String): Option[String] = Option(s).map(_.stripPrefix("-"))
}
object IsScala3 {
  def unapply(s: String): Boolean = s == "3"
}

object Parts {
  def unapplySeq(s: String): Option[Seq[String]] = Some(
    s.split('.').flatMap(_.split('-')).toList
  )
}
