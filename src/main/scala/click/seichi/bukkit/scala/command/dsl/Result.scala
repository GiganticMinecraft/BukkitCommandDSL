package click.seichi.bukkit.scala.command.dsl

import click.seichi.bukkit.scala.command.internal.generic.Result
import click.seichi.bukkit.scala.command.internal.generic.Result.{Failed, Success}

object Result {
  val succeed: Result[Nothing, Unit] = Success()

  val failWithoutMessage: Result[None.type, Nothing] = Failed(None)
  def failWithMessage(string: String): Result[Some[String], Nothing] = Failed(Some(string))
}
