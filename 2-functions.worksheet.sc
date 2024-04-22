// Arguments can be passed to functions using one of the 2 conventions: by-name (lazy) or by-value

// by-value - arg needs to be calculated before passsing to the function
def stringLength(arg: String): Int = {
  println("Calculating string length")
  val length = arg.length
  println(s"$arg has length $length")
  length
}

// by-name ( => ) - arg would be calcuated lazily upon the first usage
// Caution! If the by-name arguments is defined as a function it would be evaluated every time it would be used in the function body
def byNameStringLength(arg: => String): Int = {
  println("executed by name")
  byNameStep2(arg)
}
def byNameStep2(arg: => String): Int = {
  println("In second step of by name string length")
  stringLength(arg)
}

def makeNoisyString = {
  println("creating string...")
  "random string"
}
def noisyString = makeNoisyString

stringLength(noisyString)
byNameStringLength(noisyString)

val msg = "Hello world!"
// Lambda is alternative name for an anonymous function
val x = (n: Int) => println("hello! " * n)
x(3)
// Closure is any function which uses a free-variables - variables which are not passed directly to the function via arguments
// free-variable is defined somewhere in the context of the function, in our case it's val msg: String
val y = (n: Int) => println(msg * n)
y(3)

// Each function can be assigned to a variable
val functionAsValue: String => Int = stringLength
functionAsValue(msg)

// Partial function is a function which does not define a result all it's possible arguments
// It can define diffrent results depending on the input
val partialFunction: PartialFunction[String, String] = {
  case "SECRET"               => "It's confidential"
  case str if str.length > 10 => "Oh come on, it's to long!!!"
}

partialFunction("SECRET")
partialFunction("Hello" * 10)

// Currying
// Functions can take multiple arguments lists
def replacePrefixWithSuffix(prefix: String)(on: String) =
  on.stripPrefix(prefix) + prefix
replacePrefixWithSuffix("This Function")("This Function Is Curried")

val replaceThis = replacePrefixWithSuffix("This")(_)
replaceThis("This Function is Value")

// Default and named parameters

// Functions can define a defualt arguments, they would be used if none or only some of the arguments would be specified
def buildGreeting(who: String = "Stranger", withGift: Option[String] = None) = {
  s"Hello $who" + withGift.fold(", I have nothing for you :<")(gift => s",  here is my gift - $gift")
}

// Usage of default variant
buildGreeting()
buildGreeting("Joe")
// Passing arguments using named parameters - you can rearange order of arguments by specifing their name
buildGreeting(who = "Steve")
buildGreeting(withGift = Some("chocolates"))
buildGreeting(withGift = Some("a new assignment!"), who = "students")

// Tail recursive functions
// A function which either returns value or calls itself as the last operation can be optimized to a tail recursive loop
// In practive tail recursive function can be rewritten into while loop
// This allows to resolve Stack Overflow exception which can exist in standard recursive calls
type Num = BigInt
def fiboncacii(n: Int): Num = {
  val cache = scala.collection.mutable.Map.empty[Int, Num]
  def cached(n: Int) = cache.getOrElseUpdate(n, loop(n))
  def loop(n: Int): Num = n match {
    case 0 => 0
    case 1 => 1
    case n =>
      println(n)
      // Not tail recursive, after calling itself needs to operate on returned value
      cached(n - 2) + cached(n - 1)
  }
  loop(n)
}

def fiboncaciiTailRec(n: Int): Num = {
  // This annotation ensurers that function is tail recursive, if it is not, compiler would throw error
  @scala.annotation.tailrec
  def loop(n: Int, current: Num, prev: Num): Num = n match {
    case 0 => current
    case n => loop(n - 1, current = prev + current, prev = current)
  }
  if n <= 0 then 0
  else loop(n, prev = 1, current = 0)
}

val fibDepth = 10000
fiboncaciiTailRec(fibDepth) // Works fine
fiboncacii(fibDepth) // Stack overflow
// Non recusrsive version stack overflow in big depth, around n=7500

// Alternative using lazy list for cached fibonacii
val fibS: LazyList[Num] = 0 #:: fibS.scan(1: Num)(_ + _)
// val fibS: LazyList[Num] =
//   (0: Num) #:: (1: Num) #::
//     (fibS zip fibS.tail).map { (cur, next) => cur + next }
fibS(fibDepth)
