package org.openmole.gui.server.core

/*
 * Copyright (C) 22/09/14 // mathieu.leclaire@openmole.org
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

import java.io.File

import org.eclipse.jetty.server.Server
import org.eclipse.jetty.servlet.DefaultServlet
import org.eclipse.jetty.webapp.WebAppContext
import org.openmole.misc.pluginmanager.PluginManager
import org.openmole.misc.workspace.Workspace
import org.scalatra.servlet.ScalatraListener
import org.osgi.framework.Bundle
import scala.collection.JavaConverters._
import javax.servlet.ServletContext
import org.scalatra._
import org.openmole.misc.tools.io.FileUtil
import org.openmole.misc.tools.io.FileUtil._
//import org.eclipse.jetty.util.resource.{ Resource ⇒ Res }

class GUIServer(bundles: List[Bundle], port: Option[Int], optimized: Boolean = true) {

  println("Generated JS with" + { if (!optimized) " no" else "" } + " optimization")

  val p = port getOrElse 8080

  val webui = Workspace.file("webui")
  val jsSrc = new File(webui, "js/src")
  val jsCompiled = new File(webui, "js/compiled")
  val webapp = new File(webui, "webapp")
  jsSrc.mkdirs
  jsCompiled.mkdirs
  webapp.mkdirs

  //Copy fixed webapp resources in Workspace/webapp
  val classLoader = getClass.getClassLoader

  //classLoader.getResourceAsStream("js").copy(new File(webapp, "js"))

  //jsSrc.updateIfChanged(JSPack(bundles, jsSrc, jsCompiled)) // in fileservice
  JSPack(bundles, jsSrc, jsCompiled, optimized)

  val server = new Server(p)

  val context = new WebAppContext()

  context.setContextPath("/")
  // context.setBaseResource(Res.newResource(classOf[GUIServer].getResource("/")))
  context.setResourceBase(webapp.getAbsolutePath)
  context.setClassLoader(classOf[GUIServer].getClassLoader)
  context.addEventListener(new ScalatraListener)

  server.setHandler(context)

  def start() = {
    println("start !!!")
    server.start
    server.join
  }

  def end() {
    server.stop
    server.join
  }
}