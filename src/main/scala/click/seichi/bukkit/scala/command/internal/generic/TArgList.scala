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

  private def uncons[T](list: List[T]): Option[(T, List[T])] = list.headOption.map(head => (head, list.tail))

  trait MappableTAList[TAL <: TArgList, Trail] {
    type MapResult[_] <: TArgList
    def mapTrailHead[B](conversion: Trail => Option[B])(argList: TAL): Option[MapResult[B]]
  }

  implicit def trailMappable[Trail]: MappableTAList[Tr[Trail], Trail] { type MapResult[B] = B ::: Tr[Trail] } = {
    new MappableTAList[Tr[Trail], Trail] {
      override type MapResult[B] = B ::: Tr[Trail]
      override def mapTrailHead[B](conversion: Trail => Option[B])(argList: Tr[Trail]): Option[B ::: Tr[Trail]] = {
        for {
          (head, list) <- uncons(argList.list)
          newHead <- conversion(head)
        } yield newHead ::: Trail(list)
      }
    }
  }

  implicit def consMappable[H, T <: TArgList, Trail](implicit ev: MappableTAList[T, Trail]): MappableTAList[H ::: T, Trail] { type MapResult[B] = H ::: ev.MapResult[B] } = {
    new MappableTAList[H ::: T, Trail] {
      override type MapResult[B] = H ::: ev.MapResult[B]
      override def mapTrailHead[B](conversion: Trail => Option[B])(argList: H ::: T): Option[MapResult[B]] = {
        ev.mapTrailHead(conversion)(argList.xs).map(result => argList.x ::: result)
      }
    }
  }

  implicit class MappableTrail[GivenTrail](trail: Tr[GivenTrail]) {
    def mapTrailHead[B](conversion: GivenTrail => Option[B]): Option[B ::: Tr[GivenTrail]] = {
      trailMappable[GivenTrail].mapTrailHead(conversion)(trail)
    }
  }

  implicit class MappableTACons[H, T <: TArgList](list: H ::: T) {
    def mapTrailHead[B, Trail](conversion: Trail => Option[B])(implicit ev: MappableTAList[T, Trail]): Option[H ::: ev.MapResult[B]] = {
      consMappable(ev).mapTrailHead(conversion)(list)
    }
  }
}
