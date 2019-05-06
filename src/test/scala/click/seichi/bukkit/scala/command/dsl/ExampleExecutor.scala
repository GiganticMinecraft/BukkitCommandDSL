package click.seichi.bukkit.scala.command.dsl

import org.bukkit.entity.Player
import org.bukkit.plugin.java.JavaPlugin
import click.seichi.bukkit.scala.command.dsl.DSL._
import click.seichi.bukkit.scala.command.dsl._
import click.seichi.bukkit.scala.command.internal.generic.{Failed, Success}

class Plugin extends JavaPlugin

object ExampleExecutor {
  def repeatMessageExecutor: DSLBasedCommandExecutor = {
    configureCommand
      .canBeExecutedByPlayers
      .transformArgumentsWith(Identity ::: IntParser ::: Nil)
      .executionWithContext { context ->
        context.arguments match {
          case message ::: number ::: Nil =>
            if (number > 0) {
              for (_ <- 1 to number) context.sender.sendMessage(message)
              Success
            } else {
              // it could have been validated earlier but it's possible to do it now too
              Failed("number must be positive!")
            }
        }
      }
  }

  def killPlayerExecutor(implicit plugin: Plugin): DSLBasedCommandExecutor = {
    configurePlayerCommand()
      .transformArgumentsWithTabCompletions(onlinePlayerNameCompletor ::: Nil)
      .executionWithContext(context -> {
        val player: Player = context.arguments.head
        player.setHealth(0.0D)
        player.sendMessage(s"You've been killed by ${context.sender.getName()}!")
      })
  }

  def exampleExecutorParent(implicit plugin: Plugin) = {
    branchedCommand(
      "repeatmsg" -> repeatMessageExecutor,
      "killplayer" -> killPlayerExecutor
    )()
  }
}
