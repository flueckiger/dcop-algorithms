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
import com.signalcollect._
import com.signalcollect.dcop.modules._

class SimpleDcopVertex[Id, Action, Config <: SimpleConfig[Id, Action, UtilityType, Config], UtilityType](
  initialState: Config with SimpleConfig[Id, Action, UtilityType, Config])(
    override val optimizer: Optimizer[Id, Action, Config, UtilityType],
    debug: Boolean = false)
  extends DcopVertex[Id, Action, Config, UtilityType](initialState)(optimizer, debug) {
  
  type Signal = Action
  
  override def currentConfig: Config =
    state.collect(totalSignalMap)
}
