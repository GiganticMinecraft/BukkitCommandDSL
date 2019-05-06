name := "BukkitCommandDSL"

version := "0.1.0"

scalaVersion := "2.12.8"

resolvers ++= Seq(
  "spigot repository" at "https://hub.spigotmc.org/nexus/content/repositories/snapshots/",
  "bungeecord repository" at "https://oss.sonatype.org/content/repositories/snapshots",
  Resolver.sonatypeRepo("releases"),
)

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.0")

libraryDependencies ++= Seq(
  "org.spigotmc" % "spigot-api" % "1.13.2-R0.1-SNAPSHOT",
  "org.typelevel" %% "cats-core" % "1.6.0"
)

