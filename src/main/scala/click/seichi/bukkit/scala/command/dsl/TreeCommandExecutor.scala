package click.seichi.bukkit.scala.command.dsl

import java.util

import click.seichi.bukkit.scala.command.internal.generic.Result
import click.seichi.bukkit.scala.command.internal.generic.Result._
import org.bukkit.command.{Command, CommandSender, TabExecutor}

import scala.collection.immutable.ListMap
import scala.language.implicitConversions

abstract class TreeCommandExecutor extends TabExecutor {
  def executeWithRawContext(context: UntypedArgsCommandContext[CommandSender]): Result[Option[String], Unit]

  def getCompletionCandidatesFor(context: TabCompletionContext): List[String]

  final override def onCommand(sender: CommandSender, command: Command, label: String, args: Array[String]): Boolean = {
    val context: UntypedArgsCommandContext[CommandSender] = UntypedArgsCommandContext(sender, command, label, args.toList)

    executeWithRawContext(context) match {
      case Failed(Some(error)) => sender.sendMessage(error); false
      case Failed(None) => false
      case _ => true
    }
  }

  final override def onTabComplete(sender: CommandSender, command: Command, alias: String, args: Array[String]): util.List[String] = {
    import scala.collection.JavaConverters._

    val context = TabCompletionContext(sender, command, alias, args.toList)
    getCompletionCandidatesFor(context).asJava
  }
}

case class BranchedCommandExecutor(routing: ListMap[String, TreeCommandExecutor],
                                   branchMissingErrorMessage: Option[String] = None) extends TreeCommandExecutor {

  private def getExecutorFrom(args: List[String]): Option[TreeCommandExecutor] = for {
    branchName <- args.headOption
    executor <- routing.get(branchName)
  } yield executor

  override def executeWithRawContext(context: UntypedArgsCommandContext[CommandSender]): Result[Option[String], Unit] = {
    getExecutorFrom(context.args)
      .map(_.executeWithRawContext(context.copy(args = context.args.tail)))
      .getOrElse(Failed(branchMissingErrorMessage))
  }

  override def getCompletionCandidatesFor(context: TabCompletionContext): List[String] = {
    getExecutorFrom(context.args)
      .map(_.getCompletionCandidatesFor(context.copy(args = context.args.tail)))
      .getOrElse(Nil)
  }

}
