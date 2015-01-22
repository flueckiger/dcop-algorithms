package com.signalcollect.dcop.optimizers

import scala.util.Random
import com.signalcollect.dcop.modules._
import com.signalcollect.dcop.impl._
import com.signalcollect.dcop.impl.ArgmaxADecisionRule

class FadingMemoryJsfpiVertexColoring[AgentId, Action, UtilityType](changeProbability: Double, rhoValue: UtilityType)(implicit utilEv: Numeric[UtilityType]) extends Optimizer[AgentId, Action, SimpleMemoryConfig[AgentId, Action, UtilityType], UtilityType] {
  val schedule = new ParallelRandomAdjustmentSchedule[AgentId, Action, SimpleMemoryConfig[AgentId, Action, UtilityType]](changeProbability)
  val rule = new ArgmaxADecisionRule[AgentId, Action, SimpleMemoryConfig[AgentId, Action, UtilityType], UtilityType] 
    with NashEquilibriumConvergence[AgentId, Action, SimpleMemoryConfig[AgentId, Action, UtilityType], UtilityType] 
    with DiscountedExpectedUtilityTargetFunction[AgentId, Action, UtilityType] 
    with VertexColoringUtility[AgentId, Action, SimpleMemoryConfig[AgentId, Action, UtilityType], UtilityType] { 
        override val utilEv = FadingMemoryJsfpiVertexColoring.this.utilEv
        def rho = rhoValue
      }
  override def toString = "FadingMemoryJsfpiVertexColoringChangeProbability" + changeProbability + "rhoValue" + rhoValue
}

class JsfpiVertexColoring[AgentId, Action, UtilityType](changeProbability: Double)(implicit utilEv: Fractional[UtilityType]) extends Optimizer[AgentId, Action, SimpleMemoryConfig[AgentId, Action, UtilityType], UtilityType] {
  val schedule = new ParallelRandomAdjustmentSchedule[AgentId, Action, SimpleMemoryConfig[AgentId, Action, UtilityType]](changeProbability)
  val rule = new ArgmaxADecisionRule[AgentId, Action, SimpleMemoryConfig[AgentId, Action, UtilityType], UtilityType] 
    with NashEquilibriumConvergence[AgentId, Action, SimpleMemoryConfig[AgentId, Action, UtilityType], UtilityType] 
    with AverageExpectedUtilityTargetFunction[AgentId, Action, UtilityType] 
    with VertexColoringUtility[AgentId, Action, SimpleMemoryConfig[AgentId, Action, UtilityType], UtilityType] {
        override val utilEv = JsfpiVertexColoring.this.utilEv
      } 
  override def toString = "JsfpiVertexColoringChangeProbability" + changeProbability 
}
