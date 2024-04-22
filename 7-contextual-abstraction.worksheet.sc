import scala.language.implicitConversions

case class MyContext(databaseURL: String)

def start() =
  // Keyword 'given' allows you to define a value which would be implicitlly available in the given context
  given ctx: MyContext = MyContext("https://prod.db/api")
  // Context is being implicitlly passed to the function call
  functionUsingContext(42)
  // Explicitlly passing context to function
  functionUsingContext(42)(using MyContext("https://test.db/api/v2/"))

// Using declares that given function uses implicit instance of MyContext
def functionUsingContext(arg: Int)(using MyContext) =
  // Summon instance of MyContext from implicitlly passed value
  val ctx = summon[MyContext]
  println(s"Summoned ctx: ${ctx}, arg: $arg")
  someOtherCalculations()

def someOtherCalculations()(using ctx: MyContext) =
  println(s"got by param context: ${ctx}")
  42

start()

// Context bound [T: Bound] can be defined for any generic type for which we want to have a matching implicit instance of Bound[T]
def add[T: Numeric](left: T, right: T): T =
  summon[Numeric[T]].plus(left, right)

// Internally it's an equivalent of given function
def addEquivalent[T](left: T, right: T)(using numeric: Numeric[T]): T =
  numeric.plus(left, right)

add(1L, -1L) == addEquivalent(1L, -1L)
// add("hello", "world") // No Numeric implicit instance for String

// Context functions
// Context function ensures existance of implicit value of given type in the function type
type Contextual[T] = MyContext ?=> T

case class User(name: String)
def listUsers(): Contextual[List[User]] = ctx ?=> {
  println(s"get ${ctx.databaseURL}/users")
  User("random") :: Nil
}

def checkDB(): Contextual[Unit] = {
  println(s"check db: ${summon[MyContext].databaseURL}")
  listUsers().foreach(println)
  println("::end of audit::")
}

checkDB()(using MyContext("https://prod.env.com/api/v1"))

// Opaque types define an alias for a given type
// At the compilation it ensures type-safety, eg. you cannot assign Logarith to Double
// At runtime the have no overhead and are e.g. stored as a regular Double
object LogarithmsSupport:
  opaque type Logarithm = Double
  object Logarithm:
    // These are the two ways to lift to the Logarithm type
    def apply(d: Double): Logarithm = math.log(d)
    def safe(d: Double): Option[Logarithm] =
      if d > 0.0 then Some(math.log(d))
      else None
  end Logarithm

  // Extension methods define opaque types' public APIs
  // Extension methods allow to define a method which is not defined within the given class, but
  // from the users perspective it looks like it does
  extension (x: Logarithm)
    def toDouble: Double = math.exp(x)
    def +(y: Logarithm): Logarithm = Logarithm(math.exp(x) + math.exp(y))
    def *(y: Logarithm): Logarithm = x + y

import LogarithmsSupport._

val log10 = Logarithm(10)
val log2 = Logarithm(2)

val logs = log10 * log2
val addsLogs = log10 + log2
logs.toDouble
addsLogs.toDouble

// val d: Double = log2 // type-error!

// Conversions
// Implicit conversion is a powerfull mechanism which allows to implicitlly convert instance of given type to some other type
// when it's needed
// In this case we fill specification of kubernetes job, it's API defines multiple optional fields, which leads to a lot of boilerplate
// in order to convert String to Option[String]
case class JobSpec(
    name: Option[String] = None,
    namespace: Option[String] = None,
    command: Option[String] = None
)
JobSpec(command = Some("./calc.sh"))
// JobSpec(command = "./calc.sh") // incorrect type
{
  // We define a rule which allows to implicitlly convert generic type T to Option[T]
  // It's recommended to limit the scope where the implicit converion can be applied
  given toOption[T]: Conversion[T, Option[T]] with
    def apply(v: T): Option[T] = Some(v)

  // String is implicitlly wrapped into Option
  JobSpec(name = "customName", namespace = None, command = "echo status")
}
