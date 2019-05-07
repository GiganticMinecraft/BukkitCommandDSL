package click.seichi.bukkit.scala.command.dsl

import click.seichi.bukkit.scala.command.internal.generic.Result.{Failed, Success}
import click.seichi.bukkit.scala.command.internal.generic.{:::, Result, TArgList, Trail}
import click.seichi.bukkit.scala.command.internal.generic.TArgListOps._

object TransformationBuilder {
  def startTransformationWith[B](transformation: ArgumentTransformation[B]): Trail[String] => Result[Option[String], B ::: Trail[String]] = { trail =>
    trail.mapTrailHead(transformation.function) match {
      case Some(transformedArg) => Success(transformedArg)
      case None => Failed(transformation.errorOnFail)
    }
  }

  implicit class ArgumentsTransformation[TAL <: TArgList](existingTransformation: Trail[String] => Result[Option[String], TAL]) {
    def thenTransformWith[B](newTransformation: ArgumentTransformation[B])
                            (implicit ev: TrailHeadMappable[TAL, String]): Trail[String] => Result[Option[String], ev.MapResult[B]] = { rawArgList =>
      existingTransformation(rawArgList) match {
        case Success(typedArgList) =>
          ev.mapTrailHead(newTransformation.function)(typedArgList) match {
            case Some(transformedArg) => Success(transformedArg)
            case None => Failed(newTransformation.errorOnFail)
          }
        case f @ Failed(_) => f
      }
    }
  }
}