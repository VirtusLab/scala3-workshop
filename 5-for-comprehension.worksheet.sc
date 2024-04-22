val lines = List("hello", "world", "content:value123", "and", "something other")
def getContent(): Option[String] = lines.find(_.startsWith("content"))
def getData(str: String): Option[String] =
  Option(str)
    .map(_.stripPrefix("content:"))
    .filter(_.nonEmpty)

val result = for
  content <- getContent()
  data <- getData(content)
  if data.length > 5
yield data

val resultEquivalent = getContent()
  .flatMap(getData(_))
  .withFilter(_.length > 5)
  .map(identity)

// for-comprehension can be used for every type which does define methodmap(f: In => Out): Out
// if for-comprehension uses nested args it needs to also define a flatMap[F](f: In => F[Out]): F[Out] method
// if for-comprehenison is using filters given type also needs to define method 'withFilter'
// Replacing yield with do replaces final `map` operation with `foreach`
for
  content <- getContent()
  data <- getData(content)
  if data.length > 5
do println(data)

getContent()
  .flatMap(getData(_))
  .withFilter(_.length > 5)
  .foreach(println)
