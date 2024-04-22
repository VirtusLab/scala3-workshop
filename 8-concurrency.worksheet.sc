import scala.concurrent.*
import scala.concurrent.duration.*
import scala.util.{Failure, Success}

// Execution context is responsible for scheduling execution of Future's and
// maintaing threads pool in which the execution would be performed
given ExecutionContext = ExecutionContext.Implicits.global

// CAUTION - when executing this script via scala-cli run it might finish with errors
// To fix this wrap expressions within a function and call it
// When executing as a worksheet it should work fine

val f = Future {
  println(s"I'm in thread - ${Thread.currentThread}")
  Thread.sleep(100)
  // sys.error("panic!")
  println(s"I'm (probablly) in different thread - ${Thread.currentThread}")
  42
}

// register a callback upon future completion
// Promise is a wrapper for a value that might have not been completed yet
val callbackResult = Promise[String]
f.onComplete {
  case Success(v) =>
    println("Future completed with success")
    callbackResult.complete(Success(s"Finished, result: $v"))
  case Failure(reason) =>
    println("Future completed with failure")
    callbackResult.complete(
      Failure(RuntimeException(s"Computation failed - $reason"))
    )
}

// Block until Future is completed, return Future[T]
Await.ready(f, 1.second)
// Block until Future is completed, retunr result T
println("IsCompleted? " + callbackResult.isCompleted)
Await.result(callbackResult.future, 5.second)
println(s"Completed with $callbackResult")

case class User(name: String)
def getUsers(): Future[List[User]] = Future {
  println("gettings users")
  Thread.sleep(500)
  0.until(10).map(idx => User(s"user$idx")).toList
}

def updateUsers(users: List[User]): Future[Unit] = {
  // Future travers applies a given function returning Future to all elements in colleciton
  // Scheduling is handled by ExecutionContext
  // Typically only n=<available cpus> are being executed concurrentlly
  Future
    .traverse(users)(updateSingleUser(_))
    .map(_ => println("All users updated!"))
}

def updateSingleUser(user: User): Future[Unit] = Future {
  Thread.sleep(100 + util.Random.nextInt(500))
  println(s"Updated $user")
}

def notifyUserChange(user: User): Future[Unit] = Future {
  Thread.sleep(100 + util.Random.nextInt(500))
  println(s"Notifing external service about change of $user")
}

// Chaining Futures sequentially
val task = for {
  users <- getUsers()
  _ <- updateUsers(users)
  // starts when getUsers is done - execution is sequenced
  _ <- Future.traverse(users)(notifyUserChange)
} yield "done"

Await.result(task, 5.seconds)

// Execution in parallel
val taskPar = for {
  users <- getUsers()
  // starts when getUsers call and notifing users in parallel
  _ <- updateUsers(users).zip(Future.traverse(users)(notifyUserChange))
} yield "done"
Await.result(taskPar, 5.seconds)
