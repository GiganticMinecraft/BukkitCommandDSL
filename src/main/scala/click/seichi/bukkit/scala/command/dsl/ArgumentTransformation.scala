package click.seichi.bukkit.scala.command.dsl

case class ArgumentTransformation[B](function: String => Option[B], errorOnFail: Option[String])
