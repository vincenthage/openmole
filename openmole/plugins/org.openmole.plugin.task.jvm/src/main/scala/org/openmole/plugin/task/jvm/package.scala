/*
 * Copyright (C) 2012 Romain Reuillon
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
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.openmole.plugin.task

import java.io.File
import org.openmole.core.dsl._

package jvm {

  import org.openmole.core.exception.UserBadDataError
  import org.openmole.core.pluginmanager.PluginManager

  trait JVMPackage extends external.ExternalPackage {
    lazy val libraries = new {
      /**
       * Add a library and make it available to the task
       *
       * For instance addLib("/tmp/malib.jar") in a groovy task make the content of the
       * jar available to the task. This method support jars but has some limitation. The
       * best way to use your own bytecode (java, scala, groovy, jython) in OpenMOLE is
       * building an OpenMOLE plugin (see the section on openmole.org for details).
       *
       * @param l a jar file
       *
       */
      def +=[T: JVMLanguageBuilder](l: File*) =
        implicitly[JVMLanguageBuilder[T]].libraries.modify(_ ++ l)
    }

    lazy val plugins = new {
      def +=[T: JVMLanguageBuilder](plugins: Seq[File]*) = {
        plugins.flatten.foreach {
          plugin ⇒
            PluginManager.bundle(plugin) match {
              case None ⇒ throw new UserBadDataError(s"Plugin $plugin is not loaded")
              case _    ⇒
            }
        }
        implicitly[JVMLanguageBuilder[T]].plugins.modify(_ ++ plugins.flatten)
      }
    }
    def pluginsOf(o: Any): Seq[File] = pluginsOf(o.getClass)
    def pluginsOf[T](implicit m: Manifest[T]): Seq[File] = pluginsOf(manifest[T].runtimeClass)
    def pluginsOf(clazz: Class[_]): Seq[File] = PluginManager.pluginsForClass(clazz).toSeq
  }
}

package object jvm extends JVMPackage