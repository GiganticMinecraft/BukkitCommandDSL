package click.seichi.bukkit.scala.command.dsl

import click.seichi.bukkit.scala.command.internal.generic.TArgList
import org.bukkit.command.{Command, CommandSender}

trait CommandContext[+CS <: CommandSender] {
  val sender: CS
  val command: Command
  val aliasUsed: String
}

case class UntypedArgsCommandContext[+CS <: CommandSender](sender: CS,
                                                           command: Command,
                                                           aliasUsed: String,
                                                           args: List[String]) extends CommandContext[CS]

case class TypedArgsCommandContext[+CS <: CommandSender, +Args <: TArgList](sender: CS,
                                                                            command: Command,
                                                                            aliasUsed: String,
                                                                            args: Args) extends CommandContext[CS]

case class TabCompletionContext(sender: CommandSender, command: Command, alias: String, args: List[String])
