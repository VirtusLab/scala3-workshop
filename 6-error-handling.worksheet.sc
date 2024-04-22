def maybeFail() = {
  2 match {
    case 0 => "zero"
    case 1 => throw RuntimeException("panic!")
    // MatchError for other arguments
  }
}

var data: Option[java.io.InputStream] = None
try
  println("I'm in a try block")
  maybeFail()
catch {
  case ex: MatchError => s"caught match error - ${ex.getMessage}"
  case ex: Exception  => s"catched $ex"
} finally {
  println("After try block, cleaning stuff now")
  data.foreach(_.close)
}

import scala.util.{Try, Success, Failure}

Try {
  println("I'm in a util.Try class")
  maybeFail()
} match {
  case Success(v) => s"Successfy calculated '$v'"
  case Failure(reason) =>
    reason match {
      case ex: MatchError => s"caught match error - ${ex.getMessage}"
      case ex: Exception  => s"catched $ex"
    }
}

val tryResult = Try {
  println("I'm in a util.Try class")
  maybeFail()
}
  .map(v => s"Successfy calculated '$v'")
  .recover {
    case ex: MatchError => s"caught match error - ${ex.getMessage}"
    case ex: Exception  => s"catched $ex"
  }

// I want default value incase of failure
tryResult.getOrElse("My defaultValue")
// I don't care why it failed, just tell either the result exists
tryResult.toOption.fold("empty")("got: " + _)

tryResult.toEither.fold("failed with: " + _, "got: " + _)

tryResult.fold("failed with: " + _, "got: " + _)
