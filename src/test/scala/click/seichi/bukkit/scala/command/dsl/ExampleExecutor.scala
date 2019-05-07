package click.seichi.bukkit.scala.command.dsl

import click.seichi.bukkit.scala.command.dsl.CommandBuilder._
import click.seichi.bukkit.scala.command.dsl.Result._
import click.seichi.bukkit.scala.command.dsl.TransformationBuilder._
import click.seichi.bukkit.scala.command.internal.generic.:::
import org.bukkit.entity.Player
import org.bukkit.plugin.java.JavaPlugin

class Plugin extends JavaPlugin


object ExampleExecutor {
  val identity: String => Option[String] = Some[String]
  val intParser: String => Option[Int] = { string =>
    try { Some(string.toInt) } catch { case _: Throwable => None }
  }
  val positiveIntParser: String => Option[Int] = string => for {
    parsedInt <- intParser(string) if parsedInt > 0
  } yield parsedInt

  def repeatMessageExecutor: TreeCommandExecutor = {
    configureCommand
      .canBeExecutedBy[Player]
      .argTransformations(
        transformFirstArgument(withoutErrorOnFailure(identity))
          .thenTransformNext(withErrorOnFailure(positiveIntParser, "second argument must be a positive number."))
      )
      .executionWithContext { context =>
        context.args match { case message ::: number ::: _ =>
          for (_ <- 1 to number) context.sender.sendMessage(message)
          succeed
        }
      }
  }

  def playerFromName(implicit plugin: JavaPlugin): String => Option[Player] = { playerName =>
    Option(plugin.getServer.getPlayer(playerName))
  }

  def killPlayerExecutor(implicit plugin: Plugin): TreeCommandExecutor = {
    configureCommand
      .canBeExecutedBy[Player]
      .argTransformations(
        transformFirstArgument(withErrorOnFailure(playerFromName, "Player name invalid!"))
      )
      .executionWithContext(context => {
        val player: Player = context.args.head

        player.setHealth(0.0D)
        player.sendMessage(s"You've been killed by ${context.sender.getName}!")

        succeed
      })
  }

  def exampleExecutorParent(implicit plugin: Plugin): BranchedCommandExecutor = {
    branchedCommand(
      "repeatmsg" -> repeatMessageExecutor,
      "killplayer" -> killPlayerExecutor
    )
  }

  def main(args: Array[String]): Unit = {

  }
}
