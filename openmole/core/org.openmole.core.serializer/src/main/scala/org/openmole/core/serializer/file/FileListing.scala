/*
 * Copyright (C) 02/10/13 Romain Reuillon
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

package org.openmole.core.serializer.file

import org.openmole.tool.file._

import scala.collection.immutable.TreeSet
import java.io.File
import java.io.OutputStream
import org.openmole.core.serializer.converter.Serialiser

trait FileListing <: Serialiser {
  var listedFiles: TreeSet[File] = new TreeSet

  xStream.registerConverter(new FileConverterNotifier(this))

  def fileUsed(file: File) =
    listedFiles += file

  def toXML(obj: Any, outputStream: OutputStream) = synchronized {
    xStream.toXML(obj, outputStream)
  }

  override def clean = {
    super.clean
    listedFiles = new TreeSet
  }

}
