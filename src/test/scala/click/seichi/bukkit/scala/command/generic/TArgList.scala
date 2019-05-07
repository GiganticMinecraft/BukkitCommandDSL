package click.seichi.bukkit.scala.command.generic

import click.seichi.bukkit.scala.command.internal.generic.{:::, Trail}
import click.seichi.bukkit.scala.command.internal.generic.TArgList._
import click.seichi.bukkit.scala.command.internal.generic.TArgListOps._

object TArgList {
  val intParser: String => Option[Int] = { string =>
    try { Some(string.toInt) } catch { case _: Throwable => None }
  }

  val list1: Tr[String]                     = Trail(List("123", "456", "non number string"))
  val list2: Char ::: Tr[String]            = '0' ::: list1
  val list3: String ::: Char ::: Tr[String] = "123" ::: list2

  val list4: Option[Int ::: Tr[String]]                     = list1.mapTrailHead(intParser) // Expect 123 ::: ["456", "non number string"]
  val list5: Option[Char ::: Int ::: Tr[String]]            = list2.mapTrailHead(intParser) // Exepct '0' ::: 123 ::: ["456", "non number string"]
  val list6: Option[String ::: Char ::: Int ::: Tr[String]] = list3.mapTrailHead(intParser) // Expect "123" ::: '0' ::: 123 ::: ["456", "non number string"]

  val list7: Option[Int ::: Int ::: Int ::: Tr[String]] = for {
    l71 <- list1.mapTrailHead(intParser)
    l72 <- l71.mapTrailHead(intParser)
    l73 <- l72.mapTrailHead(intParser)
  } yield l73 // expect None

  val listPure: String => List[String] = str => List(str)

  val list8: Tr[List[String]]                       = list1.mapTrail(listPure)
  val list9: Char ::: Tr[List[String]]              = list2.mapTrail(listPure)
  val list10: String ::: Char ::: Tr[List[String]]  = list3.mapTrail(listPure)
}
