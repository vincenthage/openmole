package projectRoot.core

import sbt._


trait CorePlugin extends CoreDefaults with CoreMisc with CoreCore {
  private implicit val dir = file("core/plugins")

  lazy val corePlugin = Project("core-plugin", dir) aggregate (corePluginExternal, corePluginNetLogo, corePluginNetLogo4,
    corePluginNetLogo5)

  lazy val corePluginExternal = OsgiProject("org.openmole.plugin.task.external") dependsOn (coreMiscException,
    coreImpl, coreMiscWorkspace)

  lazy val corePluginNetLogo = OsgiProject("org.openmole.plugin.task.netlogo") dependsOn (coreImpl,
    coreMiscException, coreMiscWorkspace, corePluginExternal)

  //the imports disambiguates netlogo5 from netlogo4
  lazy val corePluginNetLogo4 = OsgiProject("org.openmole.plugin.task.netlogo4", 
    imports=Seq("org.nlogo.*;version=\"[4,5)\"", "*;resolution:=optional")) dependsOn (corePluginNetLogo,
    netlogo4_noscala)

  lazy val corePluginNetLogo5 = OsgiProject("org.openmole.plugin.task.netlogo5") dependsOn (corePluginNetLogo,
    netlogo5_noscala)

}
