/*
 * Copyright (C) 2012 Romain Reuillon
 * Copyright (C) 2014 Jonathan Passerat-Palmbach
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

package org.openmole.plugin.environment.slurm

import fr.iscpif.gridscale.ssh.{ SSHConnectionCache, SSHAuthentication, SSHJobService, SSHHost }
import fr.iscpif.gridscale.slurm.{ SLURMJobService ⇒ GSSLURMJobService, SLURMJobDescription }
import java.net.URI
import org.openmole.core.batch.control._
import org.openmole.core.batch.environment._
import org.openmole.core.batch.jobservice.{ BatchJob, BatchJobId }
import org.openmole.core.workspace.Workspace
import org.openmole.plugin.environment.ssh.{ ClusterJobService, SharedStorage, SSHService }
import org.openmole.core.batch.storage.SimpleStorage
import org.openmole.plugin.environment.gridscale._
import org.openmole.tool.logger.Logger
import concurrent.duration._

object SLURMJobService extends Logger

import SLURMJobService._

trait SLURMJobService extends ClusterJobService { js ⇒

  def environment: SLURMEnvironment

  val jobService = new GSSLURMJobService with SSHConnectionCache {
    def host = js.host
    def user = js.user
    def credential = js.credential
    override def port = js.port
    override def timeout = Workspace.preference(SSHService.timeout)
  }

  protected def _submit(serializedJob: SerializedJob) = {
    val (remoteScript, result) = buildScript(serializedJob)
    val jobDescription = SLURMJobDescription(
      executable = "/bin/bash",
      arguments = remoteScript,
      queue = environment.queue,
      workDirectory = serializedJob.path,
      wallTime = environment.wallTime,
      memory = Some(environment.requiredMemory),
      nodes = environment.nodes,
      coresByNode = environment.coresByNode orElse environment.threads,
      qos = environment.qos,
      gres = environment.gres.toList,
      constraints = environment.constraints.toList
    )

    val job = js.jobService.submit(jobDescription)
    Log.logger.fine(s"SLURM job [${job.slurmId}], description: \n ${jobDescription.toSLURM}")

    new BatchJob with BatchJobId {
      val jobService = js
      val id = job
      val resultPath = result
    }
  }

}
