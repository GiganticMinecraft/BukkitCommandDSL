package click.seichi.bukkit.scala.command.dsl

import click.seichi.bukkit.scala.command.internal.generic.Result.{Failed, Success}
import click.seichi.bukkit.scala.command.internal.generic.{Result, TArgList, Trail}
import org.bukkit.command.{CommandSender, TabExecutor}

import scala.collection.immutable.ListMap
import scala.reflect.ClassTag

object CommandBuilder {
  def configureCommand: SenderValidationReceiver.type = SenderValidationReceiver

  object SenderValidationReceiver {
    def canBeExecutedBy[CS <: CommandSender : ClassTag](errorOtherwise: Option[String]): ArgumentValidationReceiver[CS] = {
      val validator: CommandSender => Result[Option[String], CS] = { sender: CommandSender =>
        sender match {
          case refinedSender: CS => Success(refinedSender)
          case _ => Failed(errorOtherwise)
        }
      }

      ArgumentValidationReceiver[CS](validator)
    }

    def canBeExecutedBy[CS <: CommandSender : ClassTag]: ArgumentValidationReceiver[CS] = canBeExecutedBy[CS](None)
  }

  case class ArgumentValidationReceiver[CS <: CommandSender](senderValidator: CommandSender => Result[Option[String], CS]) {
    def argTransformations[Args <: TArgList](argValidator: Trail[String] => Result[Option[String], Args]): ExecutionReceiverWithoutCompletion[CS, Args] = {
      val validator: List[String] => Result[Option[String], Args] = { list => argValidator(Trail(list)) }
      ExecutionReceiverWithoutCompletion(senderValidator, validator)
    }
  }

  case class ExecutionReceiverWithoutCompletion[CS <: CommandSender, Args <: TArgList]
  (senderValidator: CommandSender => Result[Option[String], CS],
   argValidator: List[String] => Result[Option[String], Args]) {
    def executionWithContext(execution: TypedArgsCommandContext[CS, Args] => Result[Option[String], Unit]): TreeCommandExecutor = {
      new TreeCommandExecutor {
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

  def fromTabExecutor(executor: TabExecutor): TreeCommandExecutor = {
    new TreeCommandExecutor {
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

  def branchedCommand(branchMissingErrorMessage: Option[String], routing: (String, TreeCommandExecutor)*): BranchedCommandExecutor = {
    BranchedCommandExecutor(ListMap(routing: _*), branchMissingErrorMessage)
  }

  def branchedCommand(routing: (String, TreeCommandExecutor)*): BranchedCommandExecutor = branchedCommand(None, routing: _*)
}
