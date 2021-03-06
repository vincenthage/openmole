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

package org.openmole.plugin.task.systemexec

import org.openmole.core.tools.service.OS
import org.openmole.plugin.task.external._
import org.openmole.core.workflow.data._

import monocle.Lens

trait SystemExecTaskBuilder[T] extends ReturnValue[T]
    with ErrorOnReturnValue[T]
    with StdOutErr[T]
    with EnvironmentVariables[T]
    with WorkDirectory[T] { builder ⇒

  def commands: Lens[T, Vector[OSCommands]]
}
