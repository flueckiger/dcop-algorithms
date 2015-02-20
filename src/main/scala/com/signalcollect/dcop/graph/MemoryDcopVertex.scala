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

/**
 * A Memory Dcop vertex.
 *
 * @param id The Vertex Id
 * @param domain The variable Domain
 * @param optimizer The optimizer used
 * @param initialState Initial state of the vertex
 * @param debug Boolean idicating if there should be any printlines
 * @param convergeByEntireState Boolean indicating if the algorithm stops when the entire state or only the action stabilizes.
 */
class MemoryDcopVertex[Id, Action, Config <: MemoryConfig[Id, Action, UtilityType, Config], UtilityType](
  initialState: Config with MemoryConfig[Id, Action, UtilityType, Config])(
    override val optimizer: Optimizer[Id, Action, Config, UtilityType],
    debug: Boolean = false,
    eps: (Int, Int) = (1, Int.MaxValue),
    convergeByEntireState: Boolean = true)(
      implicit utilEv: Fractional[UtilityType])
  extends DcopVertex[Id, Action, Config, UtilityType](initialState)(
    optimizer, debug) {
  val epsNum = utilEv.fromInt(eps._1)
  val epsDen = utilEv.fromInt(eps._2)

  //Initialize state memory and stuff: (initialAction, Map.empty[Action, Double].withDefaultValue(0), 0)

  type Signal = Action //(Action, Map[Action, Double], Long)

  override def currentConfig: Config = {
    val neighborhood: Map[Id, Action] = totalSignalMap
    val oldC = state.collect(neighborhood)
    val newMemory = optimizer.rule.computeExpectedUtilities(oldC)
    val c = oldC.collect(neighborhood, newMemory) //TODO???
    c
  }

  def sameMaps(newMap: Map[Action, UtilityType], oldMap: Map[Action, UtilityType]): Boolean = {
    for (elem1 <- newMap) {
      val inSecondMapValue = oldMap.getOrElse(elem1._1, utilEv.fromInt(-1))
      if (utilEv.abs(elem1._2 - inSecondMapValue) >= epsNum / epsDen) return false
    }
    true
  }

  //  //TODO: Should the whole memory be the same, or only for the current action? DAAAAH!!! Same for ranked!!!
  override def isStateUnchanged(oldConfig: Config, newConfig: Config): Boolean = {
    (oldConfig.centralVariableAssignment == newConfig.centralVariableAssignment) &&
      (oldConfig.neighborhood == newConfig.neighborhood) &&
      sameMaps(oldConfig.memory, newConfig.memory)
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
