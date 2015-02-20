package com.signalcollect.dcop.optimizers

import scala.util.Random
import com.signalcollect.dcop.modules._
import com.signalcollect.dcop.impl._
import com.signalcollect.dcop.impl.ArgmaxADecisionRule

class WrmiVertexColoring[AgentId, Action, Config <: MemoryConfig[AgentId, Action, UtilityType, Config], UtilityType](changeProbability: Double, rhoValue: UtilityType)(implicit utilEv: Numeric[UtilityType]) extends Optimizer[AgentId, Action, Config, UtilityType] {
  val schedule = new ParallelRandomAdjustmentSchedule[AgentId, Action, Config](changeProbability)
  val rule = new LinearProbabilisticDecisionRule[AgentId, Action, Config, UtilityType] 
    with NashEquilibriumConvergence[AgentId, Action, Config, UtilityType] 
    with DiscountedAverageRegretsTargetFunction[AgentId, Action, Config, UtilityType] 
    with VertexColoringUtility[AgentId, Action, Config, UtilityType] {
    override val utilEv = WrmiVertexColoring.this.utilEv
    def rho = rhoValue
  }
  override def toString = "WrmiVertexColoringChangeProbability" + changeProbability + "rhoValue" + rhoValue
}
