/** Everyting in Scala is typed https://docs.scala-lang.org/tour/unified-types.html Any / \ AnyVal AnyRef Int Long Unit
  * Float Double | String Option <custom types> \ | / Nothing
  */

// AnyVal is a base type of all primitive values
var anyVal: AnyVal = -1
anyVal = 1
anyVal = 1L
anyVal = 1.0f
anyVal

// AnyRef is a base type of all the reference types
var anyRef: AnyRef = new {}
anyRef = "Hello"
anyRef = Some(1)
anyRef = List(1, 2, 3)

// Any is supertype of AnyVal and AnyRef
var any: Any = ()
any = anyVal
any = anyRef

// Unit declares no result (void from Java or C)
anyVal = ()

// Nothing is an abstract type describing an expression which never yields any value, not even an Unit
// Nothing is only a typer construct, it can never be achived in the runtime
// In practice Nothing is a type for expressions throwing exceptions
def fail: Nothing = throw new RuntimeException("Failed")
def notImplemented: Nothing = ???
// Nothing is a subtype of each possible type, it can be assigned any variable
// any = fail

// Custom types
// class always defines a main class constructor,
// In this case constructor takes 2 arguments (Int, String), the arguments would be used to create a private fields foo: Int, and bar: String in the class
class MyClass(foo: Int, bar: String = "") {
  // Auxilary constructor needs to call main or other auxilary constructor as the first expression
  def this(string: String) =
    this(string.length, string)
    println("I've used an auxilary construcotr")

  private val fooBar = s"$foo$bar"

  def length = bar.length

  // All expressions which are not definitions/declarations of methods and fields would be evaluated while exectuing the main constructor
  // They should be treated as part of main constructor
  println("MyClass is beining initialized")
}

// object allows to define a class which has only 1 instance (singleton type)
// if object has the same name as the class defined in the same file we call it the companion object
object MyClass {
  // All of the fields and methods withing object are treated as statically accessible (static in Java)
  final val DefaultName = "MyClassConstantString"
  def createInstance(n: Int) = new MyClass(n, DefaultName)
}

// Every class can inherit from only 1 other class
abstract class Foo(x: Int) {
  def foo: Int
  def fooString = foo.toString + x
}

// trait defines an abstract type defining both abstract and implemented methods and fields
// Each class can inherit from multiple traits
trait Bar(x: Int) {
  def bar: Int
  def barString = bar.toString + x
}

// class inheriting from abstract class and from trait
class FooBar extends Foo(0) with Bar(42) {
  // It's a good practive to use 'override' keyword when implementing abstract field/method, but it's optional
  override def foo: Int = ???
  override def bar: Int = ???
  // Overriding non-abstract method required using the override keyword, otherwise compiler would throw error
  override val barString = "FooBar"
}
// class FooBar extends Bar(0) with Foo(42) // order matters
new FooBar()

// traits/classes declared with `sealed` modifier can be only defined withing the same file
sealed trait Contact
// Case classes are distinct from reqular classess in a few cases
// - fields of the constructor would be become public fields, instead of being private
// - methods equals, hashCode, toString are automatically overriden based on the main constructor arguments
// - automatically define unapply method in the companion object allowing to extract main constructor arguments in pattern matching
// - defines an apply method in companion object, allowing to skip 'new' keyword
// - case class cannot inherit from other case class
// In some aspects case classess are similiar to Records in Java
case class Person(name: String, age: Int) extends Contact
case object Organization extends Contact

val contact: Contact = Person("Steve", 40)
contact match {
  case Person("Steve", _) => "Hey Steve"
  case Organization       => "..."
  case _                  => "Hey stranger"
}

sealed trait Animal
// By default all the types are public (in Java they are package private)
private class Dog(breed: String) extends Animal
protected object Elephant extends Animal
// to define a package private method use private[package] syntax
// private[somepackage] abstract class Cat extends Animal

// Enums
enum Color(val hex: Int):
  case Red extends Color(0xff0000)
  case Green extends Color(0x0000ff)
  case Blue extends Color(0x00ff00)

val color = Color.Green
val hexColor = color.hex

enum Shape:
  case Circe, Square, Triangle

// Type aliases
type Meter = Double
val distance: Meter = 10

// Union/intersecion types types
trait A {
  def doA = 2
}
trait B {
  def doB = 4
}
type AOrB = A | B
type AWithB = A & B

def doSomething(v: AOrB) = v match {
  case v: A => v.doA
  case v: B => v.doB
}

def doSomething(v: AWithB) = v.doA + v.doB

doSomething(new A {})
doSomething(new B {})
doSomething(new A with B)

// Generic types
// Generic types can be used both for data structures and methods by specifing generic type symbols in the square brackets
def log[T](v: T) = println(s"It's $v")
log("a message")
log(42.0)

trait Pet extends Animal {
  def name: String
}
case class Crocodile(name: String) extends Animal
case class Cat(name: String) extends Pet
case class Hamster(name: String) extends Pet

// Type guards can be enfored on the generic type
// T needs to be a subtype of Pet
def pet[T <: Pet](toPet: T) = {
  println(s"Petting $toPet with name ${toPet.name}")
}

pet(Cat("Alex"))
// pet(Crocodile("T-rex"))
pet(new Dog("Golden Retriver") with Pet { def name: String = "Scruffy" })

// Variance of generic types - Invariance, covariance, contravariance

// By default generic types are defined as Invariant (similary as in Java)
// Even though Cat is a subtype of Pet, Container[Cat] is not a Container[Pet]
// Also Container[Pet] is not a Container[Cat]
class Container[A](var value: A)
val catContainer = new Container(Cat("Whiskers"))
// val petContainer: Container[Pet] = catContainer // it would no compile
// petContainer.value = Hamster("")
val cat: Cat = catContainer.value // We would get a Hamster instead of Cat here

// List is an example of covariant class (List[+T])
// It means that List[Cat] is a subtype of List[Pet]
// List[Pet] is not a List[Cat]
val cats: List[Cat] = List(Cat("Whiskers"), Cat("Tom"))
val hamsters: List[Hamster] = List(Hamster("Fido"), Hamster("Rex"))
val pets: List[Pet] = cats // We can assign List[Cats] to List[Pet]
val allPets: List[Pet] =
  cats ++ hamsters // Or combine both List[Cats] and List[Pet] into a single list
allPets.map(_.name)
// List[Pet] =!= List[Cat], we cannot assign pets to List[Cat]
// val petsAsCats: List[Cat] = pets

// Contravariance is a reversed Covariance
// Printer[Cat] is not a subtype of Printer[Pet]
// Printer[Pet] is a subtype of Printer[Cat]
abstract class Printer[-T] {
  def print(value: T): Unit
}
class PetPrinter extends Printer[Pet] {
  def print(pet: Pet): Unit =
    println("The pet's name is: " + pet.name)
}

class CatPrinter extends Printer[Cat] {
  def print(cat: Cat): Unit =
    println("The cat's name is: " + cat.name)
}
def printMyCat(printer: Printer[Cat], cat: Cat): Unit =
  printer.print(cat)

val catPrinter: Printer[Cat] = new CatPrinter
val petPrinter: Printer[Pet] = new PetPrinter
// We cannot assign Printer[Cat] to Printer[Pet]
// val petPrinter: Printer[Pet] = catPrinter

val cat1 = Cat("Boots")
printMyCat(catPrinter, cat1)
printMyCat(petPrinter, cat1)
// When Printer#T is defined as invariant Printer[T] it would not compile

def printMyPet(printer: Printer[Pet], pet: Pet): Unit = printer.print(pet)
printMyPet(petPrinter, cat1)
// printMyPet(catPrinter, cat1) // Would not compile
