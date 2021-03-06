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

package org.openmole.plugin.task

import java.io.{ File, IOException, PrintStream }

import org.apache.commons.exec.CommandLine
import org.openmole.core.exception.InternalProcessingError
import org.openmole.core.tools.service.OS
import org.openmole.core.tools.service.ProcessUtil._
import org.openmole.core.workflow.data.{ Context, Prototype, RandomProvider, Variable }
import org.openmole.core.workflow.tools.{ FromContext, ExpandedString }
import org.openmole.tool.stream.StringOutputStream
import org.openmole.core.workflow.dsl._

import language.implicitConversions
import monocle.Lens
import org.openmole.plugin.task.external.External

package systemexec {

  /**
   * Command line representation
   *
   * @param command the actual command line to be executed
   */
  case class Command(command: String)

  object Command {
    /** Make commands non-remote by default */
    implicit def stringToCommand(s: String) = Command(s)
  }

  /**
   * Sequence of commands for a particular OS
   *
   * @param os    target Operating System
   * @param parts Sequence of commands to be executed
   * @see Command
   */
  case class OSCommands(os: OS, parts: Command*) {
    @transient lazy val expanded = parts.map(c ⇒ ExpandedString(c.command))
  }

  object OSCommands {
    /** A single command can be a sequence  */
    implicit def stringToCommands(s: String) = OSCommands(OS(), s)

    /** A sequence of command lines is considered local (non-remote) by default */
    implicit def seqOfStringToCommands(s: Seq[String]): OSCommands = OSCommands(OS(), s.map(s ⇒ Command(s)): _*)
  }

  import org.openmole.core.workflow.builder.InputOutputBuilder
  import org.openmole.plugin.task.external.ExternalPackage

  trait ReturnValue[T] {
    def returnValue: Lens[T, Option[Prototype[Int]]] // = None
  }

  trait ErrorOnReturnValue[T] {
    def errorOnReturnValue: Lens[T, Boolean] // = true
  }

  trait StdOutErr[T] {
    def stdOut: Lens[T, Option[Prototype[String]]] // = None
    def stdErr: Lens[T, Option[Prototype[String]]] // = None
  }

  trait EnvironmentVariables[T] {
    def environmentVariables: Lens[T, Vector[(Prototype[_], String)]]
  }

  trait WorkDirectory[T] {
    def workDirectory: Lens[T, Option[String]] // = None
  }

  trait SystemExecPackage extends ExternalPackage {

    lazy val errorOnReturnValue =
      new {
        def :=[T: ErrorOnReturnValue](b: Boolean) =
          implicitly[ErrorOnReturnValue[T]].errorOnReturnValue.set(b)
      }

    lazy val returnValue =
      new {
        def :=[T: ReturnValue](v: OptionalArgument[Prototype[Int]]) =
          implicitly[ReturnValue[T]].returnValue.set(v)
      }

    lazy val stdOut =
      new {
        def :=[T: StdOutErr](v: OptionalArgument[Prototype[String]]) =
          implicitly[StdOutErr[T]].stdOut.set(v)
      }

    lazy val stdErr =
      new {
        def :=[T: StdOutErr](v: OptionalArgument[Prototype[String]]) =
          implicitly[StdOutErr[T]].stdErr.set(v)
      }

    lazy val commands = new {
      def +=[T: SystemExecTaskBuilder](os: OS, cmd: Command*): T ⇒ T =
        (implicitly[SystemExecTaskBuilder[T]].commands add OSCommands(os, cmd: _*))
    }

    lazy val environmentVariable =
      new {
        /**
         * Add variable from openmole to the environment of the system exec task. The
         * environment variable is set using a toString of the openmole variable content.
         *
         * @param prototype the prototype of the openmole variable to inject in the environment
         * @param variable the name of the environment variable. By default the name of the environment
         *                 variable is the same as the one of the openmole protoype.
         */
        def +=[T: EnvironmentVariables: InputOutputBuilder](prototype: Prototype[_], variable: OptionalArgument[String] = None): T ⇒ T =
          (implicitly[EnvironmentVariables[T]].environmentVariables add prototype → variable.getOrElse(prototype.name)) andThen
            (inputs += prototype)
      }

    lazy val customWorkDirectory =
      new {
        def :=[T: WorkDirectory](s: OptionalArgument[String]) =
          implicitly[WorkDirectory[T]].workDirectory.set(s)
      }

  }
}

package object systemexec extends external.ExternalPackage with SystemExecPackage {

  private[systemexec] def pack = this

  object ExecutionResult {
    def empty = ExecutionResult(0, None, None)
    def append(e1: ExecutionResult, e2: ExecutionResult) =
      ExecutionResult(
        e2.returnCode,
        appendOption(e1.output, e2.output),
        appendOption(e1.errorOutput, e2.errorOutput)
      )

    private def appendOption(o1: Option[String], o2: Option[String]) =
      (o1, o2) match {
        case (None, None)         ⇒ None
        case (o1, None)           ⇒ o1
        case (None, o2)           ⇒ o2
        case (Some(v1), Some(v2)) ⇒ Some(v1 + v2)
      }
  }

  case class ExecutionResult(returnCode: Int, output: Option[String], errorOutput: Option[String])

  def commandLine(
    cmd:     FromContext[String],
    workDir: String,
    context: Context
  )(implicit rng: RandomProvider): Array[String] =
    CommandLine.parse(cmd.from(context + Variable(External.PWD, workDir))).toStrings

  def execute(
    command:              Array[String],
    workDir:              File,
    environmentVariables: Seq[(Prototype[_], String)],
    context:              Context,
    returnOutput:         Boolean,
    returnError:          Boolean
  ) = {
    try {

      val outBuilder = new StringOutputStream
      val errBuilder = new StringOutputStream

      val out = if (returnOutput) new PrintStream(outBuilder) else System.out
      val err = if (returnError) new PrintStream(errBuilder) else System.err

      val runtime = Runtime.getRuntime

      import collection.JavaConversions._
      val inheritedEnvironment = System.getenv.map { case (key, value) ⇒ s"$key=$value" }.toArray
      val openmoleEnvironment = environmentVariables.map { case (p, v) ⇒ v + "=" + context(p).toString }.toArray

      //FIXES java.io.IOException: error=26
      val process = runtime.synchronized {
        runtime.exec(
          command,
          inheritedEnvironment ++ openmoleEnvironment,
          workDir
        )
      }

      ExecutionResult(
        executeProcess(process, out, err),
        if (returnOutput) Some(outBuilder.toString) else None,
        if (returnError) Some(errBuilder.toString) else None
      )
    }
    catch {
      case e: IOException ⇒ throw new InternalProcessingError(
        e,
        s"""Error executing: ${command.mkString(" ")}

            |The content of the working directory was:
            |${workDir.listRecursive(_ ⇒ true).map(_.getPath).mkString("\n")}
          """.stripMargin
      )
    }
  }
}
