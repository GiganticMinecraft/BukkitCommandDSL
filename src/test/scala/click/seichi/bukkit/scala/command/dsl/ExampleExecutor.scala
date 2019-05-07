package click.seichi.bukkit.scala.command.dsl

import click.seichi.bukkit.scala.command.dsl.CommandConfiguration._
import click.seichi.bukkit.scala.command.dsl.Result._
import org.bukkit.entity.Player
import org.bukkit.plugin.java.JavaPlugin

class Plugin extends JavaPlugin


object ExampleExecutor {
  val identity: String => Option[String] = Some[String]
  val intParser: String => Option[Int] = { string =>
    try { Some(string.toInt) } catch { case _: Throwable => None }
  }

  def repeatMessageExecutor: TreeCommandExecutor = {
    configureCommand
      .canBeExecutedBy[Player]()
      .argTransformations(
        transformWith(identity) then transformWith(intParser)
      )
      .executionWithContext {
        context =>
          context.args match {
          case message ::: number ::: _ =>
            if (number > 0) {
              for (_ <- 1 to number) context.sender.sendMessage(message)

              succeed
            } else {
              // it could have been validated earlier but it's possible to do it now too
              failWithMessage("number must be positive!")
            }
        }
      }
  }

  def playerFromName(implicit plugin: JavaPlugin): String => Option[Player] = { playerName =>
    Option(plugin.getServer.getPlayer(playerName))
  }

  def killPlayerExecutor(implicit plugin: Plugin): TreeCommandExecutor = {
    configureCommand
      .canBeExecutedBy[Player]()
      .argTransformations(
        validateWith(playerFromName)
      )
      .executionWithContext(context => {
        val player: Player = context.args.head
        player.setHealth(0.0D)
        player.sendMessage(s"You've been killed by ${context.sender.getName()}!")

        succeed
      })
  }

  def exampleExecutorParent(implicit plugin: Plugin) = {
    branchedCommand(
      "repeatmsg" -> repeatMessageExecutor,
      "killplayer" -> killPlayerExecutor
    )()
  }
}
