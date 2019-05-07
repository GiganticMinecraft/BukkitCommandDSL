package click.seichi.bukkit.scala.command.internal.generic

/**
  * Typed argument list.
  */
sealed trait TArgList
final case class :::[+Head, +Tail <: TArgList](x: Head, xs: Tail) extends TArgList
final case class Trail[+TrailType](list: List[TrailType]) extends TArgList

object TArgList {
  implicit class TArgListOps[T <: TArgList](tail: T) {
    def :::[H](head: H): H ::: T = click.seichi.bukkit.scala.command.internal.generic.:::(head, tail)
  }

  type Tr[A] = Trail[A]

  def trail[T](list: List[T]): Tr[T] = Trail(list)
}
