// Scala by default enforces usage of immutable values declared with val keyword
// val <variableName> :<Type>? = <value>
// The result type is optional, if its not explicitlly set the result type of variable would infered from it's right hand side
val openingMessage = "Hello world"
val otherMesage: String = "Hello Scala"
// openingMessage = "" // Kompilator nie pozwoli nam przypisać ponownie do zmienej

// In case if you need to use mutable variable declare it using var keyword
// Good practive is to treat mutable variables as advanced feature and used them only critical sections of the code
var luckyNumber = 42
luckyNumber += 1
luckyNumber
// luckyNumber = Math.sqrt(2.0)
// luckyNumber

// The function can be defined using def keyword
// def <functionName>(paramName: Type*): ResultType = {body}
// If the function does not take any arguments and does not produce any side-effects (it's a pure function)
// the arguments list can be ommited
def getSomeNumber() =
  println("getting some number")
  // In Scala return keyword is optional. Function would return the result of it's body expression
  lazyInitializedNumber // This value is the last expression, it would be returned

// Functions are a first class citizens in Scala, they can be also defined as values
// val <functionName>: (inputType*) => returnType = {body}
val addFive: Int => Int = { input =>
  input + 5
}
// We can assign function to variables
val addOp: Int => Int = addFive
// Functions created as value can be used in the same way as standard methods
addOp(10)
// Function defined as a method can be converted to value by using _ operation
val getNumber: () => Int = getSomeNumber

// lazy val allows to define a variable for which initilization can be delayed
// it would be initialized upon first usage
// It allows to cache long running calculations and to resolve cyclic dependencies
lazy val lazyInitializedNumber =
  println("creating lazy number")
  luckyNumber * 2 + 3

getSomeNumber()
getSomeNumber()

// Allowed primitive types:
val b: Byte = 10
val s: Short = 10
val c: Char = 10
val i: Int = 10
val l: Long = 10L
val f: Float = 10.0f
val d: Double = 10.0

val unit: Unit = () // Unit is the same as void in Java or C

// Character sequences / Strings
val str = "Hello"

// String interpolation
// It's possible to use variables or expression to creation of a new string, you can use for that the s-String interpolator
val str2 = s"$str Steve, it's ${(c + 12) % 24} o'clock"
// f-String interpolator allows also to define a formating for the vriables
val str3 = f"Height: $d%2.3f, unformatted $d"
// raw-String interpolator allows the treated alll characters, except $ as escape literals
val str4 = raw"/\.*$$asd"
// Triple quoted strings can be used to create multi line strings and to escape double-quotes
// .stripMargin would remove all the excess chars in each line before the margin character ('|' by default)
val str5 = """{
             |"key": "value"
             |}""".stripMargin

// Conditional excpessions
if luckyNumber < 10 then println("<10") // New syntax (Scala 3): if <cond> then <expr>
else if (luckyNumber > 50) // Old syntax Scala 2: if (<cond>) <expr>
  println(">50")
else {
  println("Between 10 and 50")
}
// if the last else block is missing it would be always replaced internally with 'else ()', though the whole expression would return Unit

// Match might be used a switch in other languages
// <cond> match {
//  case <literal> => <expr>
//  case <literal> if <cond> => <expr>
//  case _ => <expr>
// }
// By default 'match' is internally transformed to if-else conditions chain
// If the argument is an integer @switch annotation would try to force compiler to create table switch
// instead of if-else chain. It would throw compile-time error if creation of table switch would not be possible
(util.Random.nextInt(10): @scala.annotation.switch) match {
  case 0 => "Boom"
  case 1 => "Almost boom"
  case _ => "Not boom"
}

// The main purpose of match expression is patten matching, which is way more powerfull then switch
// E.g. you can match the string literals
("my-foo-bar": String) match {
  case "foo"                        => "just foo"
  case s"foo-${suffix}"             => suffix
  case v @ s"${prefix}foo${suffix}" => s"prefix=$prefix, foo, suffix=$suffix, whole=$v"
  case other                        => s"something else: $other"
}

// Or you can match on complex types or event match on it's extracted values
Option.when(true)("Hello") match {
  case Some("Hello") => "it was hello"
  case Some(msg)     => msg
  case None          => "no msg"
}

// Loops
// In Scala there is not typical for loop
// You can only iterate on collections, eg. Range
for n <- 0.until(5)
do
  luckyNumber = util.Random.nextInt(100)
  println(luckyNumber)
// It's a syntax sugar for `0.until(5).foreach(n => ...)`

// To save the results of iterations you replace for-do with for-yield
val coords = for
  i <- 0 until 5 if i % 2 == 0
  j <- 0 until 5 if j % 2 != 0
yield (i, j)
// It's a sugar syntax for:
// 0.until(5).filer(_ % 2 == 0).flatMap{i =>
//   0.until(5).filter(_ % 2 != 0).map{j =>
//     (i, j)
//   }
// }

// You can also perform a while-do loop
val coordsIterator = coords.iterator
while (coordsIterator.hasNext) {
  println(coordsIterator.next)
}

// In Scala 3 there is not longer a do-while loop,
// However it can be replaced with correctly constructed while-do loop
var iter2 = coords.iterator
while {
  val next = iter2.next
  println(next)
  iter2.hasNext && next._1 < 3
} do ()

// Input / output
println("Hello world")
// import scala.io.StdIn
// val line = StdIn.readLine()
// val boolean = StdIn.readBoolean()
