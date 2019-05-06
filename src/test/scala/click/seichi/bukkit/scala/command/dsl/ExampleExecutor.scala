package click.seichi.bukkit.scala.command.dsl

import click.seichi.bukkit.scala.command.dsl.CommandConfiguration._
import click.seichi.bukkit.scala.command.dsl.Result._
import org.bukkit.entity.Player
import org.bukkit.plugin.java.JavaPlugin

class Plugin extends JavaPlugin


object ExampleExecutor {
  def repeatMessageExecutor: TreeCommandExecutor = {
    configureCommand
      .canBeExecutedBy[Player]()
      .withArgValidations(

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

  def killPlayerExecutor(implicit plugin: Plugin): TreeCommandExecutor = {
    configureCommand
      .canBeExecutedBy[Player]()
      .withArgValidations()
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
