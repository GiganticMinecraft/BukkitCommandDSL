package click.seichi.bukkit.scala.command.dsl

import click.seichi.bukkit.scala.command.internal.generic.Result.{Failed, Success}
import click.seichi.bukkit.scala.command.internal.generic.{:::, Result, TArgList, Trail}
import click.seichi.bukkit.scala.command.internal.generic.TArgListOps._

object TransformationBuilder {
  private def transformFirstArgument[B](function: String => Option[B], errorOnFail: Option[String]): Trail[String] => Result[Option[String], B ::: Trail[String]] = { trail =>
    trail.mapTrailHead(function) match {
      case Some(transformedArg) => Success(transformedArg)
      case None => Failed(errorOnFail)
    }
  }

  def transformFirstArgWithoutError[B](transformation: String => Option[B]): Trail[String] => Result[Option[String], B ::: Trail[String]] = {
    transformFirstArgument(transformation, None)
  }

  def transformFirstArgWithError[B](transformation: String => Option[B], errorOnFail: String): Trail[String] => Result[Option[String], B ::: Trail[String]] = {
    transformFirstArgument(transformation, Some(errorOnFail))
  }

  implicit class ArgumentsTransformation[TAL <: TArgList](existingTransformation: Trail[String] => Result[Option[String], TAL]) {
    private def thenTransformNext[B](function: String => Option[B], errorOnFail: Option[String])
                            (implicit ev: TrailHeadMappable[TAL, String]): Trail[String] => Result[Option[String], ev.MapResult[B]] = { rawArgList =>
      existingTransformation(rawArgList) match {
        case Success(typedArgList) =>
          ev.mapTrailHead(function)(typedArgList) match {
            case Some(transformedArg) => Success(transformedArg)
            case None => Failed(errorOnFail)
          }
        case f @ Failed(_) => f
      }
    }

    def thenTransformNextWithoutError[B](newTransformation: String => Option[B])
                                        (implicit ev: TrailHeadMappable[TAL, String]): Trail[String] => Result[Option[String], ev.MapResult[B]] = {
      thenTransformNext(newTransformation, None)(ev)
    }

    def thenTransformNextWithError[B](newTransformation: String => Option[B], errorOnFail: String)
                                     (implicit ev: TrailHeadMappable[TAL, String]): Trail[String] => Result[Option[String], ev.MapResult[B]] = {
      thenTransformNext(newTransformation, Some(errorOnFail))(ev)
    }
  }
}
