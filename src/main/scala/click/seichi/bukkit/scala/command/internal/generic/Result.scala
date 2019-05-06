package click.seichi.bukkit.scala.command.internal.generic

sealed trait Result[+ERR, +RES]
object Result {
  case class Failed[+ERR](error: ERR) extends Result[ERR, Nothing]
  case class Success[+RES](result: RES) extends Result[Nothing, RES]

  implicit class MappableResult[ERR, RES](result: Result[ERR, RES]) {
    def flatMap[B](function: RES => Result[ERR, B]): Result[ERR, B] = result match {
      case Success(resultValue) => function(resultValue)
      case f @ Failed(_) => f
    }

    def map[B](function: RES => B): Result[ERR, B] = flatMap(res => Success(function(res)))
  }
}
