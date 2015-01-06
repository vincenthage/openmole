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

import org.openmole.core.model.data._
import org.openmole.core.model.task.PluginSet
import org.openmole.plugin.task.external._
import org.openmole.core.implementation.data._

import scala.collection.mutable.ListBuffer

class SystemExecTaskBuilder(commands: Commands)(implicit plugins: PluginSet) extends ExternalTaskBuilder { builder ⇒

  private val variables = new ListBuffer[(Prototype[_], String)]
  private val _commands = new ListBuffer[Commands]
  private var errorOnReturnValue = true
  private var returnValue: Option[Prototype[Int]] = None
  private var stdOut: Option[Prototype[String]] = None
  private var stdErr: Option[Prototype[String]] = None
  private var workDirectory: Option[String] = None

  addCommand(commands)

  /**
   * Add variable from openmole to the environment of the system exec task. The
   * environment variable is set using a toString of the openmole variable content.
   *
   * @param prototype the prototype of the openmole variable to inject in the environment
   * @param variable the name of the environment variable. By default the name of the environment
   *                 variable is the same as the one of the openmole protoype.
   */
  def addEnvironmentVariable(prototype: Prototype[_], variable: Option[String] = None): this.type = {
    variables += prototype -> variable.getOrElse(prototype.name)
    addInput(prototype)
    this
  }

  def addCommand(cmd: Commands) = {
    _commands += cmd
    this
  }

  def setErrorOnReturnValue(b: Boolean) = {
    errorOnReturnValue = b
    this
  }

  def setReturnValue(p: Option[Prototype[Int]]) = {
    returnValue = p
    this
  }

  def setStdOut(p: Option[Prototype[String]]) = {
    stdOut = p
    this
  }

  def setStdErr(p: Option[Prototype[String]]) = {
    stdErr = p
    this
  }

  def setWorkDirectory(s: Option[String]) = {
    workDirectory = s
    this
  }

  def toTask =
    new SystemExecTask(_commands.toList, workDirectory, errorOnReturnValue, returnValue, stdOut, stdErr, variables.toList) with builder.Built {
      override val outputs = builder.outputs ++ DataSet(List(stdOut, stdErr, returnValue).flatten)
    }

}