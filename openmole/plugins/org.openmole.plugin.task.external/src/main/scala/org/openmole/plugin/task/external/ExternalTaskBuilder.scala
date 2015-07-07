/*
 * Copyright (C) 2012 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
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

package org.openmole.plugin.task.external

import java.io.File
import org.openmole.core.tools.service.OS
import org.openmole.core.workflow.builder.TaskBuilder
import org.openmole.core.workflow.data._
import org.openmole.core.workflow.tools.ExpandedString
import org.openmole.core.workflow.data.Prototype
import org.openmole.plugin.task.external.ExternalTask._
import scala.collection.mutable.ListBuffer
import org.openmole.core.workflow.task.PluginSet

/**
 * Builder for task using external files or directories
 *
 * Use it to copy files or directories, from the dataflow or from your computer in the task
 * workspace and prior to the task execution and to get files generated by the
 * task after its execution.
 *
 */
abstract class ExternalTaskBuilder extends TaskBuilder { builder ⇒

  private val _inputFiles = new ListBuffer[InputFile]
  private val _inputFileArrays = new ListBuffer[InputFileArray]
  private val _outputFiles = new ListBuffer[OutputFile]
  private val _resources = new ListBuffer[Resource]

  def inputFiles = _inputFiles.toList
  def inputFileArrays = _inputFileArrays.toList
  def outputFiles = _outputFiles.toList
  def resources = _resources.toList

  /**
   * Copy a file from your computer in the workspace of the task
   *
   * @param file the file or directory to copy in the task workspace
   * @param name the destination name of the file in the task workspace, by
   * default it is the same as the original file name
   * @param link tels if the entire content of the file should be copied or
   * if a symbolic link is suitable. In the case link is set to true openmole will
   * try to use a symbolic link if available on your system.
   *
   */
  def addResource(file: File, name: Option[ExpandedString] = None, link: Boolean = false, inWorkDir: Boolean = false, os: OS = OS()): ExternalTaskBuilder.this.type = {
    _resources += Resource(file, name.getOrElse(file.getName), link, inWorkDir, os)
    this
  }

  /**
   * Copy a file or directory from the dataflow to the task workspace
   *
   * @param p the prototype of the data containing the file to be copied
   * @param name the destination name of the file in the task workspace
   * @param link @see addResouce
   *
   */
  def addInputFile(p: Prototype[File], name: ExpandedString, link: Boolean = false, inWorkDir: Boolean = true): this.type = {
    _inputFiles += InputFile(p, name, link, inWorkDir)
    this addInput p
    this
  }

  def addInputFileArray(p: Prototype[Array[File]], prefix: ExpandedString, suffix: ExpandedString = "", link: Boolean = false, inWorkDir: Boolean = true): this.type = {
    _inputFileArrays += InputFileArray(p, prefix, suffix, link, inWorkDir)
    this addInput p
    this
  }

  /**
   * Get a file generate by the task and inject it in the dataflow
   *
   * @param name the name of the file to be injected
   * @param p the prototype that is injected
   *
   */
  def addOutputFile(name: ExpandedString, p: Prototype[File], inWorkDir: Boolean = true): this.type = {
    _outputFiles += OutputFile(name, p, inWorkDir)
    this addOutput p
    this
  }

  trait Built extends super.Built {
    def inputFiles = builder.inputFiles.toList
    def inputFileArrays = builder.inputFileArrays.toList
    def outputFiles = builder.outputFiles.toList
    def resources = builder.resources.toList
  }

}
