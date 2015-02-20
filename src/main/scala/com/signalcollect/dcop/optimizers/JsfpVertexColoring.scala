package com.signalcollect.dcop.optimizers

import scala.util.Random
import com.signalcollect.dcop.modules._
import com.signalcollect.dcop.impl._
import com.signalcollect.dcop.impl.ArgmaxADecisionRule

class FadingMemoryJsfpiVertexColoring[AgentId, Action, Config <: MemoryConfig[AgentId, Action, UtilityType, Config], UtilityType](changeProbability: Double, rhoValue: UtilityType)(implicit utilEv: Numeric[UtilityType]) extends Optimizer[AgentId, Action, Config, UtilityType] {
  val schedule = new ParallelRandomAdjustmentSchedule[AgentId, Action, Config](changeProbability)
  val rule = new ArgmaxADecisionRule[AgentId, Action, Config, UtilityType] 
    with NashEquilibriumConvergence[AgentId, Action, Config, UtilityType] 
    with DiscountedExpectedUtilityTargetFunction[AgentId, Action, Config, UtilityType] 
    with VertexColoringUtility[AgentId, Action, Config, UtilityType] { 
        override val utilEv = FadingMemoryJsfpiVertexColoring.this.utilEv
        def rho = rhoValue
      }
  override def toString = "FadingMemoryJsfpiVertexColoringChangeProbability" + changeProbability + "rhoValue" + rhoValue
}

class JsfpiVertexColoring[AgentId, Action, Config <: MemoryConfig[AgentId, Action, UtilityType, Config], UtilityType](changeProbability: Double)(implicit utilEv: Fractional[UtilityType]) extends Optimizer[AgentId, Action, Config, UtilityType] {
  val schedule = new ParallelRandomAdjustmentSchedule[AgentId, Action, Config](changeProbability)
  val rule = new ArgmaxADecisionRule[AgentId, Action, Config, UtilityType] 
    with NashEquilibriumConvergence[AgentId, Action, Config, UtilityType] 
    with AverageExpectedUtilityTargetFunction[AgentId, Action, Config, UtilityType] 
    with VertexColoringUtility[AgentId, Action, Config, UtilityType] {
        override val utilEv = JsfpiVertexColoring.this.utilEv
      } 
  override def toString = "JsfpiVertexColoringChangeProbability" + changeProbability 
}
