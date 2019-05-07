package click.seichi.bukkit.scala.command.internal.generic

/**
  * Typed argument list.
  */
sealed trait TArgList
final case class :::[+Head, +Tail <: TArgList](x: Head, xs: Tail) extends TArgList
final case class Trail[+TrailType](list: List[TrailType]) extends TArgList

object TArgList {
  type Tr[A] = Trail[A]
}
