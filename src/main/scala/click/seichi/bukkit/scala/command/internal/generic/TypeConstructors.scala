package click.seichi.bukkit.scala.command.internal.generic

object TypeConstructors {
  type Id[x] = x
  type Const[a] = { type Lambda[x] = a }
}
