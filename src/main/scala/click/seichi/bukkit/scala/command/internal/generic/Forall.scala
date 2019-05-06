package click.seichi.bukkit.scala.command.internal.generic

/**
  * "Pi-type" as in System F. It allows universal application to a type of a type-constructor `F`.
  * @tparam F A type constructor to which universal application is done.
  *
  * @author kory33
  */
trait Forall[F[_]] { def apply[x]: F[x] }
