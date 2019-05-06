package click.seichi.bukkit.scala.command.internal.generic

object Transformation {
  /**
    * Family of functions from types mapped by `F` to types mapped by `G`.
    */
  type Transformation[F[_], G[_]] = Forall[Lambda[x => F[x] => G[x]]]
}
