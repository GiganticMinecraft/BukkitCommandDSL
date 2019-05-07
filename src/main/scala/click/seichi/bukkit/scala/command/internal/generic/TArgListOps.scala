package click.seichi.bukkit.scala.command.internal.generic

import click.seichi.bukkit.scala.command.internal.generic.TArgList.Tr

object TArgListOps {
  implicit class AppendableTArgList[T <: TArgList](tail: T) {
    def :::[H](head: H): H ::: T = click.seichi.bukkit.scala.command.internal.generic.:::(head, tail)
  }

  /**
    * Typeclass to witness that the head of the trail of value of type `TAL` can be mapped
    * to form another `TArgList` together with a mapped type.
    *
    * @tparam TAL a subtype of `TArgList` to have the described property
    * @tparam Trail type of the elements in the trail of `TAL`
    */
  trait TrailHeadMappable[TAL <: TArgList, Trail] {
    type MapResult[_] <: TArgList
    def mapTrailHead[B](conversion: Trail => Option[B])(argList: TAL): Option[MapResult[B]]
  }

  implicit def trailHeadMappable[Trail]: TrailHeadMappable[Tr[Trail], Trail] { type MapResult[B] = B ::: Tr[Trail] } = {
    def uncons[T](list: List[T]): Option[(T, List[T])] = list.headOption.map(head => (head, list.tail))

    new TrailHeadMappable[Tr[Trail], Trail] {
      override type MapResult[B] = B ::: Tr[Trail]
      override def mapTrailHead[B](conversion: Trail => Option[B])(argList: Tr[Trail]): Option[B ::: Tr[Trail]] = {
        for {
          (head, list) <- uncons(argList.list)
          newHead <- conversion(head)
        } yield newHead ::: Trail(list)
      }
    }
  }

  implicit def consTrailHeadMappable[H, T <: TArgList, Trail](implicit ev: TrailHeadMappable[T, Trail]): TrailHeadMappable[H ::: T, Trail] { type MapResult[B] = H ::: ev.MapResult[B] } = {
    new TrailHeadMappable[H ::: T, Trail] {
      override type MapResult[B] = H ::: ev.MapResult[B]
      override def mapTrailHead[B](conversion: Trail => Option[B])(argList: H ::: T): Option[MapResult[B]] = {
        ev.mapTrailHead(conversion)(argList.xs).map(result => argList.x ::: result)
      }
    }
  }

  implicit class TrailTrailHeadMappable[GivenTrail](trail: Tr[GivenTrail]) {
    def mapTrailHead[B](conversion: GivenTrail => Option[B]): Option[B ::: Tr[GivenTrail]] = {
      trailHeadMappable[GivenTrail].mapTrailHead(conversion)(trail)
    }
  }

  implicit class TAConsTrailHeadMappable[H, T <: TArgList](list: H ::: T) {
    def mapTrailHead[B, Trail](conversion: Trail => Option[B])(implicit ev: TrailHeadMappable[T, Trail]): Option[H ::: ev.MapResult[B]] = {
      consTrailHeadMappable(ev).mapTrailHead(conversion)(list)
    }
  }

  /**
    * Typeclass to witness that the trail of value of type `TAL` can be mapped(as in `map` in `List`) to another trail.
    *
    * @tparam TAL a subtype of `TArgList` to have the described property
    * @tparam Trail type of values in the trail
    */
  trait TrailMappable[TAL <: TArgList, Trail] {
    type MapResult[_] <: TArgList
    def mapTrail[B](function: Trail => B)(list: TAL): MapResult[B]
  }

  implicit def trailMappable[Trail]: TrailMappable[Tr[Trail], Trail] { type MapResult[B] = Tr[B] } = {
    new TrailMappable[Tr[Trail], Trail] {
      override type MapResult[B] = Tr[B]
      override def mapTrail[B](function: Trail => B)(trail: Tr[Trail]): MapResult[B] = trail match {
        case Trail(list) => Trail(list.map(function))
      }
    }
  }

  implicit def consTrailMappable[H, T <: TArgList, Trail](implicit ev: TrailMappable[T, Trail]): TrailMappable[H ::: T, Trail] { type MapResult[B] = H ::: ev.MapResult[B] } = {
    new TrailMappable[H ::: T, Trail] {
      override type MapResult[B] = H ::: ev.MapResult[B]
      override def mapTrail[B](function: Trail => B)(list: H ::: T): MapResult[B] = {
        list match {
          case head ::: tail => head ::: ev.mapTrail(function)(tail)
        }
      }
    }
  }

  implicit class TrailTrailMappable[Trail](trail: Tr[Trail]) {
    def mapTrail[B](function: Trail => B): Tr[B] = trailMappable.mapTrail(function)(trail)
  }

  implicit class TAConsTrailMappable[H, T <: TArgList](list: H ::: T) {
    def mapTrail[B, Trail](function: Trail => B)(implicit ev: TrailMappable[T, Trail]): H ::: ev.MapResult[B] = {
      consTrailMappable[H, T, Trail].mapTrail(function)(list)
    }
  }
}
