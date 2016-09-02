/*
 *  Copyright (C) 2015 Jonathan Passerat-Palmbach
 *  Copyright (C) 2016 Romain Reuillon
 *  Copyright (C) 2016 Vincent Hage
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

package org.openmole.plugin.task.container

import java.io.File

import monocle.macros.Lenses
import org.openmole.core.exception.InternalProcessingError
import org.openmole.core.workflow.builder.{ InputOutputBuilder, InputOutputConfig }
import org.openmole.core.workflow.data._
import org.openmole.core.workflow.dsl._
import org.openmole.core.workflow.task._
import org.openmole.core.workflow.tools._
import org.openmole.core.workflow.validation._
import org.openmole.core.workspace.Workspace
import org.openmole.plugin.task.external.{ External, ExternalBuilder }
import org.openmole.plugin.task.systemexec
import org.openmole.plugin.task.systemexec._
import org.openmole.tool.logger.Logger
import org.openmole.tool.tar._
import spray.json._

import scalaz._
import Scalaz._

object OCI {

  case class EmptyObject()
  object EmptyObject

  case class ManifestData(
    Config:   String,
    Layers:   List[String],
    RepoTags: List[String]
  )
  object ManifestData

  case class ConfigurationData(
    Cmd:          Option[List[String]],
    Entrypoint:   Option[List[String]],
    Env:          Option[List[String]],
    ExposedPorts: Option[Map[String, EmptyObject]],
    User:         Option[String],
    Volumes:      Option[Map[String, EmptyObject]],
    WorkingDir:   Option[String]
  )
  object ConfigurationData

  object OCIJsonProtocol extends DefaultJsonProtocol {
    implicit val emptyObjectFormat = jsonFormat0(EmptyObject.apply)
    implicit val manifestDataFormat = jsonFormat3(ManifestData.apply)
    implicit val configurationDataFormat = jsonFormat7(ConfigurationData.apply)
  }
}
import OCI._
import OCIJsonProtocol._

object WhiteoutUtils {
  /* @see <a href="https://github.com/docker/docker/blob/master/pkg/archive/whiteouts.go">Whiteout files</a> */
  /* indicates a file removed in recent layers */
  val whiteoutPrefix = ".wh."
  /* indicates special file, generally excluded from exported archives */
  val whiteoutMetaPrefix = whiteoutPrefix + whiteoutPrefix
  /* indicates a link removed in recent layers */
  val whiteoutLinkDir = whiteoutPrefix + "plnk"
  /* indicates that the current directory is inaccessible in recent layers
  * (the directory file should have a whiteout file in the parent folder) */
  val whiteoutOpaqueDir = whiteoutPrefix + ".opq."

  def isWhiteout(file: File) = file.getName.startsWith(whiteoutPrefix)

  private def getPrefix(file: File) = {
    val filename = file.getName

    if (filename.startsWith(whiteoutOpaqueDir)) whiteoutOpaqueDir
    else if (filename.startsWith(whiteoutLinkDir)) whiteoutLinkDir
    else if (filename.startsWith(whiteoutMetaPrefix)) whiteoutMetaPrefix
    else if (filename.startsWith(whiteoutPrefix)) whiteoutPrefix
    else ""
  }

  def getConcernedFile(whiteout: File): Option[File] = {
    val prefix = getPrefix(whiteout)
    // we get the concerned file by removing the prefix
    val concernedFile = new File(whiteout.getPath.replaceAll(prefix, ""))
    concernedFile.exists() match {
      case true  ⇒ Some(concernedFile)
      case false ⇒ None
    }
  }
}
import WhiteoutUtils._

object ContainerTask extends Logger {

  implicit def isTask: InputOutputBuilder[ContainerTask] = InputOutputBuilder(ContainerTask._config)
  implicit def isExternal: ExternalBuilder[ContainerTask] = ExternalBuilder(ContainerTask.external)

  implicit def isBuilder = new ContainerTaskBuilder[ContainerTask] {
    override def hostFiles = ContainerTask.hostFiles
    override def environmentVariables = ContainerTask.environmentVariables
    override def workDirectory = ContainerTask.workDirectory
    override def returnValue = ContainerTask.returnValue
    override def errorOnReturnValue = ContainerTask.errorOnReturnValue
    override def stdOut = ContainerTask.stdOut
    override def stdErr = ContainerTask.stdErr
  }

  def apply(archive: File, command: String): ContainerTask =
    new ContainerTask(
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

  //TODO: Add ports
  //TODO: Add volumes bindings
}

@Lenses case class ContainerTask(
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

  private def getManifestAndConfig(extractedDir: File): (ManifestData, ConfigurationData) = {
    val manifestFile = extractedDir / "manifest.json"
    if (!manifestFile.exists())
      throw new InternalProcessingError("Extracted image doesn't contain manifest file")

    val manifestAST = manifestFile.content.parseJson
    val manifest = manifestAST.convertTo[ManifestData]

    val configurationFile = extractedDir / manifest.Config
    if (!configurationFile.exists())
      throw new InternalProcessingError("Extracted image doesn't contain configuration file")
    val configAST = configurationFile.content.parseJson
    val config = configAST.convertTo[ConfigurationData]

    (manifest, config)
  }

  /**
   * OpenContainer archives are tarballs, with a format as such:
   *   - manifest.json
   *   - config.json  // (real name of config is inside manifest)
   *   - repositories
   *   - layer1
   *     - layer.tar
   *     - json
   *     - VERSION
   *   - ...
   *
   * What this function does is:
   *   1. extract the image tarball
   *   2. retrieve the manifest file (that contains the list of layers)
   *   3. squash the image (extract and merge the layers)
   *   4. remove the whiteouts
   */
  protected def extractArchive(taskWorkDirectory: File): File = {
    // 1. Preparing the image (extracting it)
    val extractedDir = taskWorkDirectory.newDir("image")
    archive.extract(extractedDir)

    // 2. Analysing the image
    val (manifest, config) = getManifestAndConfig(extractedDir)

    // 3. Squashing image
    val rootfsDir = extractedDir / "rootfs"
    rootfsDir.mkdir()

    // extracting all layers into the same directory, rootfs
    manifest.Layers.foreach {
      layerName ⇒
        {
          (extractedDir / layerName).extract(rootfsDir)
        }
    }

    // removing the whiteout files
    val whiteouts = rootfsDir.listRecursive(f ⇒ isWhiteout(f))
    whiteouts.foreach(whiteout ⇒ if (whiteout.exists()) {
      // whiteouts indicate that a file has been deleted in recent layers,
      // so we need to delete this one too
      getConcernedFile(whiteout) match {
        case Some(file) ⇒ file.recursiveDelete
        case None       ⇒
      }
      // and then the whiteout itself
      whiteout.recursiveDelete
    })

    extractedDir
  }

  /**
   * Return the script file that contains :
   * - the environment variables
   * - call to PRoot
   * - chroot (-r, -R or -S)
   * - the working directory (-w)
   * - the entrypoint ?
   * - other options
   */
  protected def getExecutionScript(extractedArchive: File): File = {
    val executionScript = extractedArchive / "re-execute.sh"
    executionScript.createNewFile()

    val (manifest, config) = getManifestAndConfig(extractedArchive)

    val entrypoint = config.Entrypoint match {
      case Some(list) ⇒ assembleCommandParts(list)
      case None       ⇒ ""
    }
    val workingDir = config.WorkingDir match {
      case Some(dir) ⇒ dir
      case None      ⇒ "/"
    }
    val envVariables = config.Env match {
      case Some(envMap) ⇒
        s"""
           | ${envMap.map(envEquality ⇒ s"""export $envEquality""").mkString(" \n")} \\
            """.stripMargin
      case None ⇒ ""
    }

    executionScript.content =
      s"""
         |#!/bin/bash
         |PROOT=./proot
         |$envVariables
         |$$PROOT \\
         | -R rootfs \\
         | -w $workingDir \\
         | -n \\
         | $entrypoint \\
         | $$@
          """.stripMargin

    executionScript.setExecutable(true)

    executionScript
  }

  /**
   * Not sure about this one.
   * TODO: Verify (ask to Jonathan)
   */
  protected def getUserWorkDirectory(reExecute: File): String = {
    val packagingDirectory: String = workDirectoryLine(reExecute.lines).getOrElse(
      throw new InternalProcessingError(s"Could not find packaging path in ${archive}")
    )

    workDirectory.getOrElse(packagingDirectory)
  }

  /**
   * Get the path to the PRoot executable.
   * Because it is not included in the OpenContainer images,
   * it is copied from OpenMOLE's resources directory
   */
  protected def getPRoot(extractedArchive: File): File = {
    // 4.1. We copy proot in the local extracted image
    def runtimeDirLocation = Workspace.openMOLELocation / "runtime"

    val prootLocation = runtimeDirLocation / "proot"

    if (!prootLocation.exists())
      throw new InternalProcessingError("PRoot not found in OpenMOLE runtime directory")

    val prootExecutable = extractedArchive / "proot"
    prootLocation.copy(prootExecutable)

    prootExecutable
  }

  def assembleCommandParts(args: List[String]) = {
    var command = ""
    for (arg ← args)
      command += arg + " "
    command
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
