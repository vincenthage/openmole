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

package org.openmole.plugin.method.evolution.ga

import org.openmole.plugin.method.evolution._
import fr.iscpif.mgo._

object MonoObjective {

  def apply(
    mu: Int,
    termination: GATermination { type G >: MonoObjective#G; type P >: MonoObjective#P; type F >: MonoObjective#F },
    inputs: Inputs[String],
    objective: Objective,
    reevaluate: Double = 0.0) = {
    val (_mu, _reevaluate, _inputs) = (mu, reevaluate, inputs)
    new MonoObjective {
      val inputs = _inputs
      val objectives = Seq(objective)
      val stateManifest: Manifest[STATE] = termination.stateManifest
      val populationManifest: Manifest[Population[G, P, F]] = implicitly
      val individualManifest: Manifest[Individual[G, P, F]] = implicitly
      val aManifest: Manifest[A] = implicitly
      val fManifest: Manifest[F] = implicitly
      val gManifest: Manifest[G] = implicitly

      val genomeSize = inputs.size

      override val cloneProbability: Double = _reevaluate

      val mu = _mu
      type STATE = termination.STATE
      def initialState: STATE = termination.initialState
      def terminated(population: Population[G, P, F], terminationState: STATE): (Boolean, STATE) = termination.terminated(population, terminationState)
    }
  }
}

trait MonoObjective extends GAAlgorithm
    with dynamic.DynamicApplicationGA
    with BinaryTournamentSelection
    with TournamentOnAggregatedFitness
    with DiversityAggregatedElitism
    with NoArchive
    with CloneRemoval
    with GeneticBreeding
    with MGFitness
    with ClampedGenome
    with MaxAggregation {
  type INPUT = String
  def inputConverter = implicitly
}
