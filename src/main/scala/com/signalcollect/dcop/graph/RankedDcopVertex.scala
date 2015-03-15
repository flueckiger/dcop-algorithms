/*
 *  @author Philip Stutz
 *  @author Mihaela Verman
 *  
 *  Copyright 2013 University of Zurich
 *      
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *  
 *         http://www.apache.org/licenses/LICENSE-2.0
 *  
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *  
 */

package com.signalcollect.dcop.graph

import scala.math.Fractional.Implicits._
import scala.math.Ordering.Implicits._
import com.signalcollect._
import com.signalcollect.dcop.modules._
import com.signalcollect.dcop.impl._

class RankedDcopEdge[Id, UtilityType](targetId: Id)(implicit utilEv: Fractional[UtilityType]) extends DefaultEdge(targetId) {
  type Source = Vertex[_, _ <: RankedConfig[Id, _, UtilityType, _], _, _]

  def signal = {
    val sourceState = source.state
    val sourceStateAssignment = source.state.centralVariableAssignment
    (sourceStateAssignment._2, sourceState.ranks(sourceStateAssignment._1) / utilEv.fromInt(source.edgeCount))
  }
}

object RankedDcopEdge {
  def apply[Id, Action, Config <: Configuration[Id, Action, Config], UtilityType](vertex: DcopVertex[Id, Action, Config, UtilityType])(implicit utilEv: Fractional[UtilityType]) =
    new RankedDcopEdge[Id, UtilityType](vertex.state.centralVariableAssignment._1)
}

/**
 * A Ranked Dcop vertex. Its state is composed by its action and its rank.ß
 *
 * @param id The Vertex Id
 * @param domain The variable Domain
 * @param optimizer The optimizer used
 * @param initialState Initial state of the vertex
 * @param debug Boolean idicating if there should be any printlines
 * @param convergeByEntireState Boolean indicating if the algorithm stops when the entire state or only the action stabilizes.
 */
class RankedDcopVertex[Id, Action, Config <: RankedConfig[Id, Action, UtilityType, Config], UtilityType](
  initialState: Config with RankedConfig[Id, Action, UtilityType, Config])(
    override val optimizer: Optimizer[Id, Action, Config, UtilityType],
    baseRank: (Int, Int) = (3, 20),
    unchangedMoveRankFactor: (Int, Int) = (1, 1),
    unchangedMoveRankAddend: (Int, Int) = (0, 1),
    changedMoveRankFactor: (Int, Int) = (1, 1),
    changedMoveRankAddend: (Int, Int) = (0, 1),
    debug: Boolean = false,
    eps: (Int, Int) = (1, 100000000),
    convergeByEntireState: Boolean = true)(
      implicit utilEv: Fractional[UtilityType])
  extends DcopVertex[Id, Action, Config, UtilityType](initialState)(optimizer, debug) {
  val baseRankNum = utilEv.fromInt(baseRank._1)
  val baseRankDen = utilEv.fromInt(baseRank._2)
  val unchangedMoveRankFactorNum = utilEv.fromInt(unchangedMoveRankFactor._1)
  val unchangedMoveRankFactorDen = utilEv.fromInt(unchangedMoveRankFactor._2)
  val unchangedMoveRankAddendNum = utilEv.fromInt(unchangedMoveRankAddend._1)
  val unchangedMoveRankAddendDen = utilEv.fromInt(unchangedMoveRankAddend._2)
  val changedMoveRankFactorNum = utilEv.fromInt(changedMoveRankFactor._1)
  val changedMoveRankFactorDen = utilEv.fromInt(changedMoveRankFactor._2)
  val changedMoveRankAddendNum = utilEv.fromInt(changedMoveRankAddend._1)
  val changedMoveRankAddendDen = utilEv.fromInt(changedMoveRankAddend._2)
  val epsNum = utilEv.fromInt(eps._1)
  val epsDen = utilEv.fromInt(eps._2)

  //Initialize (initialAction, baseRank: Double = 0.15,)

  type Signal = (Action, UtilityType)

  def currentConfig: Config = {
    val neighborhoodSignalMap = mostRecentSignalMap
    val neighborhoodAssignments = neighborhoodSignalMap.
      map(tuple => (tuple._1, tuple._2._1)).toMap
    val neighborhoodRanks: Map[Id, UtilityType] = neighborhoodSignalMap.
      map(tuple => (tuple._1, tuple._2._2)).toMap
    //  val ranks = neighborhoodRanks + ((id, state._2))
    val oldRanks = neighborhoodRanks + ((id, state.ranks(id)))
    val oldC = state.collect(neighborhoodAssignments, oldRanks)
    val ranks = neighborhoodRanks + ((id, computeRankForMove(oldC)))
    val c = oldC.collect(ranks)
    c
  }

  //TODO: Replace with more general.  
  def computeRankForMove(c: Config): UtilityType = {
    val allies = c.neighborhood.filterNot(c.expectedConflicts.compose(_._1))
    val allyRankSum = allies.keys.map(c.ranks).sum
    val dampingNumerator = baseRankDen - baseRankNum
    val newPageRankNumerator = baseRankNum + dampingNumerator * allyRankSum
    newPageRankNumerator / baseRankDen
  }

  def sameMaps(newMap: Map[Id, UtilityType], oldMap: Map[Id, UtilityType]): Boolean = {
    for (elem1 <- newMap) {
      val inSecondMapValue = oldMap.getOrElse(elem1._1, utilEv.fromInt(-1))
      if (utilEv.abs(elem1._2 - inSecondMapValue) >= epsNum / epsDen) return false
    }
    true
  }
  
  override def isStateUnchanged(oldConfig: Config, newConfig: Config): Boolean = {
    (oldConfig.centralVariableAssignment == newConfig.centralVariableAssignment) &&
    (oldConfig.neighborhood == newConfig.neighborhood) && 
    sameMaps(oldConfig.ranks, newConfig.ranks)
  }

  override def changeMove(c: Config): Config = {
    val agentId = c.centralVariableAssignment._1
    val ranks = c.ranks
    val move = optimizer.computeMove(c)
    val newRanks = if (move == c.centralVariableValue)
      ranks + ((agentId, ranks(agentId) *
        unchangedMoveRankFactorNum / unchangedMoveRankFactorDen +
        unchangedMoveRankAddendNum / unchangedMoveRankAddendDen))
    else
      ranks + ((agentId, ranks(agentId) *
        changedMoveRankFactorNum / changedMoveRankFactorDen +
        changedMoveRankAddendNum / changedMoveRankAddendDen))
    val newConfig = c.changeMove(move, newRanks)
    if (debug) {
      println(s"Vertex $id has changed its state from $state to $newConfig.")
    }
    newConfig
  }

  override def collect = {
    val c = currentConfig
    if (optimizer.shouldConsiderMove(c)) {
      changeMove(c)
    } else {
      if (debug) {
        if (isConverged(c)) {
          println(s"Vertex $id has converged and stays at move $c.")
        } else {
          println(s"Vertex $id still NOT converged, stays at move, and has $c.")
        }
      }
      c
    }
  }

}
