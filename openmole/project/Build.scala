import sbt._
import Keys._

import root._


object Root extends Defaults(Core, Libraries, Gui, ThirdParties, Bin, root.Doc) {
  implicit val dir = file(".")
  lazy val all = Project(id = "openmole-root", base = dir) aggregate (subProjects: _*)


  override def settings = super.settings ++ Seq(
    resolvers += Resolver.sonatypeRepo("snapshots"),
    resolvers += Resolver.sonatypeRepo("releases"),
    publish := ()
  )
}
