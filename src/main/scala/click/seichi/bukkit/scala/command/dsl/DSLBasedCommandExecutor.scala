package click.seichi.bukkit.scala.command.dsl

import java.util

import click.seichi.bukkit.scala.command.internal.generic.{Failed, Result, Success, TArgList}
import org.bukkit.command.{Command, CommandSender, TabExecutor}

import scala.collection.immutable.ListMap
import scala.language.implicitConversions
import scala.reflect.ClassTag

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

abstract class DSLBasedCommandExecutor extends TabExecutor {
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

case class BranchedCommandExecutor(routing: ListMap[String, DSLBasedCommandExecutor],
                                   branchMissingErrorMessage: Option[String] = None) extends DSLBasedCommandExecutor {

  private def getExecutorFrom(args: List[String]): Option[DSLBasedCommandExecutor] = for {
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

object DSL {
  def configureCommand: SenderValidationReceiver.type = SenderValidationReceiver

  object SenderValidationReceiver {
    def canBeExecutedBy[CS <: CommandSender : ClassTag](errorOtherwise: Option[String] = None): ArgumentValidationReceiver[CS] = {
      val validator: CommandSender => Result[Option[String], CS] = { sender: CommandSender =>
        sender match {
          case refinedSender: CS => Success(refinedSender)
          case _ => Failed(errorOtherwise)
        }
      }

      ArgumentValidationReceiver[CS](validator)
    }
  }

  // with sender validation
  case class ArgumentValidationReceiver[CS <: CommandSender](senderValidator: CommandSender => Result[Option[String], CS]) {
    // TODO define a method which receives validations on arguments and construct next builder
  }

  case class ExecutionReceiverWithoutCompletion[CS <: CommandSender, Args <: TArgList]
  (senderValidator: CommandSender => Result[Option[String], CS],
   argValidator: List[String] => Result[Option[String], Args]) {
    def executionWithContext(execution: TypedArgsCommandContext[CS, Args] => Result[Option[String], Unit]): DSLBasedCommandExecutor = {
      new DSLBasedCommandExecutor {
        override def executeWithRawContext(context: UntypedArgsCommandContext[CommandSender]): Result[Option[String], Unit] = {
          for {
            validatedSender <- senderValidator(context.sender)
            validatedArgs <- argValidator(context.args)
            typedContext = TypedArgsCommandContext(validatedSender, context.command, context.aliasUsed, validatedArgs)
            executionResult <- execution(typedContext)
          } yield executionResult
        }

        override def getCompletionCandidatesFor(context: TabCompletionContext): List[String] = Nil
      }
    }
  }

  def fromTabExecutor(executor: TabExecutor): DSLBasedCommandExecutor = {
    new DSLBasedCommandExecutor {
      override def executeWithRawContext(context: UntypedArgsCommandContext[CommandSender]): Result[Option[String], Unit] = {
        val executionResult = executor.onCommand(context.sender, context.command, context.aliasUsed, context.args.toArray)

        if (executionResult) {
          Success()
        } else {
          Failed(None)
        }
      }

      override def getCompletionCandidatesFor(context: TabCompletionContext): List[String] = {
        import scala.collection.JavaConverters._

        executor.onTabComplete(context.sender, context.command, context.alias, context.args.toArray).asScala.toList
      }
    }
  }

  def branchedCommand(routing: (String, DSLBasedCommandExecutor)*)
                     (branchMissingErrorMessage: Option[String] = None): BranchedCommandExecutor = {
    BranchedCommandExecutor(ListMap(routing: _*), branchMissingErrorMessage)
  }
}