/*
 * Copyright (C) 2014 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.openmole.plugin.environment.glite

import java.net.URI
import org.openmole.misc.workspace.Workspace

trait LCGCp {

  def voName: String

  @transient lazy val lcgCp =
    s"lcg-cp --vo ${voName} --checksum --connect-timeout $getTimeOut --sendreceive-timeout $getTimeOut --srm-timeout $getTimeOut "

  def lcgCpCmd(from: String, to: URI) = s"$lcgCp file:$from ${to.toString}"
  def lcgCpCmd(from: URI, to: String) = s"$lcgCp ${from.toString} file:$to"

  private def getTimeOut = Workspace.preferenceAsDuration(GliteEnvironment.RemoteTimeout).toSeconds.toString

}