package root

import sbt._
import org.openmole.buildsystem.OMKeys._
import sbt.Keys._

object ThirdParties extends Defaults {

  lazy val dir = file("third-parties")

  lazy val openmoleCache = OsgiProject("org.openmole.tool.cache", imports = Seq("*")) settings (bundleType := Set("core")) dependsOn (openmoleLogger)
  lazy val openmoleTar = OsgiProject("org.openmole.tool.tar", imports = Seq("*")) settings (bundleType := Set("core")) dependsOn (openmoleFile)
  lazy val openmoleFile = OsgiProject("org.openmole.tool.file", imports = Seq("*")) settings (bundleType := Set("core")) dependsOn (openmoleLock, openmoleStream, openmoleStream, openmoleLogger)
  lazy val openmoleLock = OsgiProject("org.openmole.tool.lock", imports = Seq("*")) settings (bundleType := Set("core"))
  lazy val openmoleLogger = OsgiProject("org.openmole.tool.logger", imports = Seq("*")) settings (bundleType := Set("core"))
  lazy val openmoleThread = OsgiProject("org.openmole.tool.thread", imports = Seq("*")) settings (bundleType := Set("core")) dependsOn (openmoleLogger)
  lazy val openmoleHash = OsgiProject("org.openmole.tool.hash", imports = Seq("*")) settings (bundleType := Set("core")) dependsOn (openmoleFile, openmoleStream)
  lazy val openmoleStream = OsgiProject("org.openmole.tool.stream", imports = Seq("*")) settings (bundleType := Set("core")) dependsOn (openmoleThread) settings (libraryDependencies += Libraries.collections)
  lazy val openmoleCollection = OsgiProject("org.openmole.tool.collection", imports = Seq("*")) settings (bundleType := Set("core")) settings (libraryDependencies += Libraries.scalaLang)
  lazy val openmoleCrypto = OsgiProject("org.openmole.tool.crypto", imports = Seq("*")) settings (bundleType := Set("core")) settings (libraryDependencies += Libraries.bouncyCastle)
  lazy val openmoleStatistics = OsgiProject("org.openmole.tool.statistics", imports = Seq("*")) settings (bundleType := Set("core")) dependsOn (openmoleLogger)
  lazy val openmoleTypes = OsgiProject("org.openmole.tool.types", imports = Seq("*")) settings (bundleType := Set("core")) settings (libraryDependencies += Libraries.scalaz)
  lazy val openmoleByteCode = OsgiProject("org.openmole.tool.bytecode", imports = Seq("*")) settings (bundleType := Set("core")) settings (libraryDependencies += Libraries.asm)
  lazy val openmoleOSGi = OsgiProject("org.openmole.tool.osgi", imports = Seq("*")) settings (bundleType := Set("core")) dependsOn (openmoleFile)

  lazy val txtmark = OsgiProject("com.quandora.txtmark", exports = Seq("com.github.rjeschke.txtmark.*"), imports = Seq("*")) settings (bundleType := Set("core"))

  override def settings = super.settings ++ Seq(libraryDependencies += Libraries.scalatest)
}
