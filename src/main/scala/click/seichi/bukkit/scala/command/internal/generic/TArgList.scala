package click.seichi.bukkit.scala.command.internal.generic

/**
  * Typed argument list.
  */
sealed trait TArgList
case class TArgTrail[+Trail](list: List[Trail]) extends TArgList
case class TArgCons[+Head, +Tail <: TArgList](x: Head, xs: Tail) extends TArgList

object TArgList {
  type Tr[A] = TArgTrail[A]
  type :::[A, B <: TArgList] = TArgCons[A, B]
  implicit class TListAppendable[B <: TArgList](b: B) {
    def :::[A](a: A) = TArgCons(a, b)
  }

  def trail[T](list: List[T]): Tr[T] = TArgTrail(list)
  def tArgCons[H, T <: TArgList](head: H, tail: T): H ::: T = head ::: tail

  implicit class UnConsList[T](list: List[T]) {
    def uncons: Option[(T, List[T])] = list.headOption.map(head => (head, list.tail))
  }

  trait TClass1[TAL <: TArgList] {
    type MapTrailHeadResult[B] <: TArgList
    type Trail
    def mapTrailHead[B](conversion: Trail => Option[B])(argList: TAL): Option[MapTrailHeadResult[B]]
  }

  implicit def trailTClass1[GivenTrail]: TClass1[Tr[GivenTrail]] {
    type MapTrailHeadResult[B] = B ::: Tr[GivenTrail]
    type Trail = GivenTrail
  } = new TClass1[Tr[GivenTrail]] {
    override type MapTrailHeadResult[B] = B ::: Tr[Trail]
    override type Trail = GivenTrail

    override def mapTrailHead[B](conversion: GivenTrail => Option[B])(argList: Tr[GivenTrail]): Option[B ::: Tr[Trail]] = {
      for {
        (head, list) <- argList.list.uncons
        newHead <- conversion(head)
      } yield TArgCons(newHead, TArgTrail(list))
    }
  }

  implicit def consTClass1[H, T <: TArgList](implicit ev: TClass1[T]): TClass1[H ::: T] {
    type MapTrailHeadResult[B] = H ::: ev.MapTrailHeadResult[B]
    type Trail = ev.Trail
  } = new TClass1[H ::: T] {
    override type MapTrailHeadResult[B] = H ::: ev.MapTrailHeadResult[B]
    override type Trail = ev.Trail

    override def mapTrailHead[B](conversion: Trail => Option[B])(argList: H ::: T): Option[MapTrailHeadResult[B]] = {
      ev.mapTrailHead(conversion)(argList.xs).map(result => argList.x ::: result)
    }
  }

  implicit class TrailTClass2[GivenTrail](trail: Tr[GivenTrail]) {
    def mapTrailHead[B](conversion: GivenTrail => Option[B]): Option[B ::: Tr[GivenTrail]] = {
      trailTClass1[GivenTrail].mapTrailHead(conversion)(trail)
    }
  }

  implicit class ConsTClass2[H, T <: TArgList](list: H ::: T) {
    def mapTrailHead[B](implicit ev: TClass1[T]): (ev.Trail => Option[B]) => Option[H ::: ev.MapTrailHeadResult[B]] = {
      conversion => consTClass1(ev).mapTrailHead(conversion)(list)
    }
  }

  object Test {
    val intParser: String => Option[Int] = { string =>
      try { Some(string.toInt) } catch { case _: Throwable => None }
    }

    val list1: Tr[String]                     = trail(List("123", "456", "non number string"))
    val list2: Char ::: Tr[String]            = tArgCons('0', list1)
    val list3: String ::: Char ::: Tr[String] = tArgCons("123", list2)

    val list4: Option[Int ::: Tr[String]]                     = list1.mapTrailHead(intParser)       // Expect 123 ::: ["456", "non number string"]
    val list5: Option[Char ::: Int ::: Tr[String]]            = list2.mapTrailHead.apply(intParser) // Exepct '0' ::: 123 ::: ["456", "non number string"]
    val list6: Option[String ::: Char ::: Int ::: Tr[String]] = list3.mapTrailHead.apply(intParser) // Expect "123" ::: '0' ::: 123 ::: ["456", "non number string"]

    val list7: Option[Int ::: Int ::: Int ::: Tr[String]] = for {
      l71 <- list1.mapTrailHead(intParser)
      l72 <- l71.mapTrailHead.apply(intParser)
      l73 <- l72.mapTrailHead.apply(intParser)
    } yield l73 // expect None
  }
}
