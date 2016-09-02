/*
 *  Copyright (C) 2015 Jonathan Passerat-Palmbach
 *  Copyright (C) 2016 Romain Reuillon
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Affero General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 * 
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Affero General Public License for more details.
 * 
 *  You should have received a copy of the GNU Affero General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.openmole.plugin.task.care

import java.io.File

import monocle.macros.Lenses
import org.openmole.core.exception.InternalProcessingError
import org.openmole.core.workflow.builder.{ InputOutputBuilder, InputOutputConfig }
import org.openmole.core.workflow.data._
import org.openmole.core.workflow.dsl._
import org.openmole.core.workflow.task._
import org.openmole.core.workflow.tools._
import org.openmole.core.workflow.validation._
import org.openmole.plugin.task.external.{ External, ExternalBuilder }
import org.openmole.plugin.task.systemexec
import org.openmole.plugin.task.systemexec._
import org.openmole.tool.logger.Logger

import scalaz._
import Scalaz._

object CARETask extends Logger {
  implicit def isTask: InputOutputBuilder[CARETask] = InputOutputBuilder(CARETask._config)
  implicit def isExternal: ExternalBuilder[CARETask] = ExternalBuilder(CARETask.external)

  implicit def isBuilder = new CARETaskBuilder[CARETask] {
    override def hostFiles = CARETask.hostFiles
    override def environmentVariables = CARETask.environmentVariables
    override def workDirectory = CARETask.workDirectory
    override def returnValue = CARETask.returnValue
    override def errorOnReturnValue = CARETask.errorOnReturnValue
    override def stdOut = CARETask.stdOut
    override def stdErr = CARETask.stdErr
  }

  def apply(archive: File, command: String): CARETask =
    new CARETask(
      archive = archive,
      command = command,
      hostFiles = Vector.empty,
      workDirectory = None,
      errorOnReturnValue = true,
      returnValue = None,
      stdOut = None,
      stdErr = None,
      environmentVariables = Vector.empty,
      _config = InputOutputConfig(),
      external = External()
    )

}

@Lenses case class CARETask(
    archive:              File,
    hostFiles:            Vector[(String, Option[String])],
    command:              systemexec.Command,
    workDirectory:        Option[String],
    errorOnReturnValue:   Boolean,
    returnValue:          Option[Prototype[Int]],
    stdOut:               Option[Prototype[String]],
    stdErr:               Option[Prototype[String]],
    environmentVariables: Vector[(Prototype[_], String)],
    _config:              InputOutputConfig,
    external:             External
) extends Task with ValidateTask {

  def config = InputOutputConfig.outputs.modify(_ ++ Seq(stdOut, stdErr, returnValue).flatten)(_config)

  lazy val expandedCommand = ExpandedString(command.command)

  override def validate = expandedCommand.validate(inputs.toSeq)

  archive.setExecutable(true)

  /**
   * CARE archives are auto-extractable,
   * so extracting them only requires executing them.
   */
  protected def extractArchive(taskWorkDirectory: File): File = {
    // unarchiving in task's work directory
    // no need to retrieve error => will throw exception if failing
    execute(Array(archive.getAbsolutePath), taskWorkDirectory, Seq.empty, Context.empty, true, true)

    taskWorkDirectory.listFilesSafe.headOption.getOrElse(
      throw new InternalProcessingError("Work directory should contain extracted archive, but is empty")
    )
  }

  protected def getExecutionScript(extractedArchive: File): File = {
    extractedArchive / "re-execute.sh"
  }

  protected def getUserWorkDirectory(reExecute: File): String = {
    val packagingDirectory: String = workDirectoryLine(reExecute.lines).getOrElse(
      throw new InternalProcessingError(s"Could not find packaging path in ${archive}")
    )

    workDirectory.getOrElse(packagingDirectory)
  }

  /**
   * Get the path to the PRoot executable.
   * It is already included in CARE archives.
   */
  protected def getPRoot(extractedArchive: File): File = {
    extractedArchive / "proot"
  }

  override protected def process(context: Context, executionContext: TaskExecutionContext)(implicit rng: RandomProvider) = external.withWorkDir(executionContext) { taskWorkDirectory ⇒
    taskWorkDirectory.mkdirs()

    val extractedArchive = extractArchive(taskWorkDirectory)

    val reExecute = getExecutionScript(extractedArchive)

    def userWorkDirectory = getUserWorkDirectory(reExecute)

    def inputPathResolver(path: String) = {
      if (new File(path).isAbsolute) taskWorkDirectory / "inputs" / path
      else taskWorkDirectory / "inputs" / userWorkDirectory / path
    }

    val preparedContext = external.prepareInputFiles(context, inputPathResolver)

    /** Replace new proot with a version with user bindings */
    val proot = getPRoot(extractedArchive)
    proot move (extractedArchive / "proot.origin")

    /** Traverse directory hierarchy to retrieve terminal elements (files and empty directories) */
    def leafs(file: File, bindingDestination: String): Seq[(File, String)] =
      if (file.isDirectory)
        if (file.isDirectoryEmpty) Seq(file → bindingDestination)
        else file.listFilesSafe.flatMap(f ⇒ leafs(f, s"$bindingDestination/${f.getName}"))
      else Seq(file → bindingDestination)

    /** Each input file receives a binding */
    def bindings =
      leafs(taskWorkDirectory / "inputs", "").map { case (f, b) ⇒ f.getAbsolutePath → b } ++
        hostFiles.map { case (f, b) ⇒ f → b.getOrElse(f) }

    def createDestination(binding: (String, String), baseDir: String = "rootfs") = {
      import org.openmole.tool.file.{ File ⇒ OMFile }
      val (f, b) = binding

      if (OMFile(f).isDirectory) (OMFile(baseDir) / b).mkdirs()
      else {
        val dest = OMFile(baseDir) / b
        dest.getParentFileSafe.mkdirs()
        dest.createNewFile()
      }
    }

    for (binding ← bindings) createDestination(binding)

    // replace original proot executable with a script that will first bind all the inputs in the guest rootfs before
    // calling the original proot
    proot.content =
      s"""
        |#!/bin/bash
        |TRUEPROOT="$${PROOT-$$(dirname $$0)/proot.origin}"
        |$${TRUEPROOT} \\
        | ${bindings.map { case (f, d) ⇒ s"""-b "$f:$d"""" }.mkString(" \\\n")} \\
        | $$@
      """.stripMargin

    proot.setExecutable(true)

    reExecute.content = reExecute.lines.map {
      case line if line.trim.startsWith("-w") ⇒ s"-w '$userWorkDirectory' \\"
      case line                               ⇒ line
    }.mkString("\n")

    reExecute.setExecutable(true)

    val commandline = commandLine(expandedCommand.map(s"./${reExecute.getName} " + _), userWorkDirectory, preparedContext)

    // FIXME duplicated from SystemExecTask
    val executionResult = execute(commandline, extractedArchive, environmentVariables, preparedContext, stdOut.isDefined, stdErr.isDefined)
    if (errorOnReturnValue && returnValue.isEmpty && executionResult.returnCode != 0)
      throw new InternalProcessingError(
        s"""Error executing command":
                 |[${commandline.mkString(" ")}] return code was not 0 but ${executionResult.returnCode}""".stripMargin
      )

    def rootDirectory = extractedArchive / "rootfs"

    def outputPathResolver(filePath: String): File = {
      def isAbsolute = new File(filePath).isAbsolute
      if (isAbsolute) rootDirectory / filePath else rootDirectory / userWorkDirectory / filePath
    }

    val retContext = external.fetchOutputFiles(preparedContext, outputPathResolver)
    external.checkAndClean(this, retContext, taskWorkDirectory)

    retContext ++
      List(
        stdOut.map { o ⇒ Variable(o, executionResult.output.get) },
        stdErr.map { e ⇒ Variable(e, executionResult.errorOutput.get) },
        returnValue.map { r ⇒ Variable(r, executionResult.returnCode) }
      ).flatten
  }
}
