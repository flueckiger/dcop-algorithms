package com.signalcollect.dcop.optimizers

import scala.util.Random
import com.signalcollect.dcop.modules._
import com.signalcollect.dcop.impl._
import com.signalcollect.dcop.impl.ArgmaxADecisionRule

class WrmiVertexColoring[AgentId, Action, UtilityType](changeProbability: Double, rhoValue: UtilityType)(implicit utilEv: Numeric[UtilityType]) extends Optimizer[AgentId, Action, SimpleMemoryConfig[AgentId, Action, UtilityType], UtilityType] {
  val schedule = new ParallelRandomAdjustmentSchedule[AgentId, Action, SimpleMemoryConfig[AgentId, Action, UtilityType]](changeProbability)
  val rule = new LinearProbabilisticDecisionRule[AgentId, Action, SimpleMemoryConfig[AgentId, Action, UtilityType], UtilityType] 
    with NashEquilibriumConvergence[AgentId, Action, SimpleMemoryConfig[AgentId, Action, UtilityType], UtilityType] 
    with DiscountedAverageRegretsTargetFunction[AgentId, Action, UtilityType] 
    with VertexColoringUtility[AgentId, Action, SimpleMemoryConfig[AgentId, Action, UtilityType], UtilityType] {
    override val utilEv = WrmiVertexColoring.this.utilEv
    def rho = rhoValue
  }
  override def toString = "WrmiVertexColoringChangeProbability" + changeProbability + "rhoValue" + rhoValue
}
