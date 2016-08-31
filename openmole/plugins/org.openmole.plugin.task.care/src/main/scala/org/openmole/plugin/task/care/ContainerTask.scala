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

package org.openmole.plugin.task.care

import java.io.File
import java.nio.file.Files

import org.openmole.core.exception.InternalProcessingError
import org.openmole.core.workflow.builder.InputOutputConfig
import org.openmole.core.workspace.Workspace
import org.openmole.plugin.task.external.External
import org.openmole.tool.file._
import org.openmole.tool.logger.Logger
import org.openmole.tool.tar._
import spray.json._

import scala.util.Try

object ContainerTask extends Logger {

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
  import OCI.OCIJsonProtocol._
  import OCI._

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
      Files.exists(concernedFile) match {
        case true  ⇒ Some(concernedFile)
        case false ⇒ None
      }
    }
  }

  import WhiteoutUtils._

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
    ) {

      var configurationFileTmp: Option[File] = None

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
      override protected def extractArchive(taskWorkDirectory: File): Try[File] = {
        // 1. Preparing the image (extracting it)
        val extractedDir = taskWorkDirectory.newDir("image")
        archive.extract(extractedDir)

        // 2. Analysing the image
        val manifestFile = extractedDir / "manifest.json"
        if (!manifestFile.exists())
          throw new InternalProcessingError("Extracted image doesn't contain manifest file")

        val manifestAST = manifestFile.content.parseJson
        val manifest = manifestAST.convertTo[ManifestData]

        configurationFileTmp = Some(extractedDir / manifest.Config)

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

        Try(extractedDir)
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
      override protected def getExecutionScript(extractedArchive: File): Try[File] = {
        val executionScript = extractedArchive / "re-execute.sh"
        executionScript.createNewFile()

        val configurationFile: File = configurationFileTmp match {
          case Some(file) ⇒ file
          case None       ⇒ throw new InternalProcessingError("Extracted image doesn't contain configuration file")
        }

        if (!configurationFile.exists())
          throw new InternalProcessingError("Extracted image doesn't contain configuration file")
        val configAST = configurationFile.content.parseJson
        val config = configAST.convertTo[ConfigurationData]

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

        Try(executionScript)
      }

      /**
       * Not sure about this one.
       * TODO: Verify (ask to Jonathan)
       */
      override protected def getUserWorkDirectory(reExecute: File): Try[String] = {
        val packagingDirectory: String = workDirectoryLine(reExecute.lines).getOrElse(
          throw new InternalProcessingError(s"Could not find packaging path in ${archive}")
        )

        Try(workDirectory.getOrElse(packagingDirectory))
      }

      /**
       * Get the path to the PRoot executable.
       * Because it is not included in the OpenContainer images,
       * it is copied from OpenMOLE's resources directory
       */
      override protected def getPRoot(extractedArchive: File): Try[File] = {
        // 4.1. We copy proot in the local extracted image
        def runtimeDirLocation = Workspace.openMOLELocation / "runtime"

        val prootLocation = runtimeDirLocation / "proot"

        if (!prootLocation.exists())
          throw new InternalProcessingError("PRoot not found in OpenMOLE runtime directory")

        val prootExecutable = extractedArchive / "proot"
        prootLocation.copy(prootExecutable)

        Try(prootExecutable)
      }

      def assembleCommandParts(args: List[String]) = {
        var command = ""
        for (arg ← args)
          command += arg + " "
        command
      }
    }

  //TODO: Add ports
  //TODO: Add volumes bindings
}
