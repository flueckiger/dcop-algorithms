package com.signalcollect.dcop.optimizers

import com.signalcollect.dcop.modules._
import com.signalcollect.dcop.impl._

class DsanVertexColoring[AgentId, Action, Config <: Configuration[AgentId, Action]](changeProbability: Double, constant: Double, kval: Double) extends Optimizer[AgentId, Action, Config, Double] {
  val schedule = new ParallelRandomAdjustmentSchedule[AgentId, Action, Config](changeProbability)
  val rule = new SimulatedAnnealingDecisionRule[AgentId, Action, Config] 
    with NashEquilibriumConvergence[AgentId, Action, Config] 
    with MemoryLessTargetFunction[AgentId, Action, Config, Double] 
    with VertexColoringUtility[AgentId, Action, Config] { 
      def const = constant 
      def k = kval 
      }
  override def toString = "DsanVertexColoringChangeProbability" + changeProbability + "const" + constant + "k" + kval
}

//case class DsanVertexColoring(changeProbability: Double, const: Double, k: Double)
//  extends SimpleDcopAlgorithm {
//  val schedule = new ParallelRandomAdjustmentSchedule(changeProbability)
//  val rule = new SimulatedAnnealingDecisionRule with NashEquilibriumConvergence with MemoryLessTargetFunction with VertexColoringUtility { def const = const; def k = k }
//  override def toString = "DsaAVertexColoringChangeProbability" + changeProbability + "const" + const + "k" + k
//}
//
///**
// * Ranked, RankedConflict, NoRankConflict, DynamicRankedConflict
// */
//
//case class RankedDsanVertexColoring(changeProbability: Double, const: Double, k: Double)
//  extends RankedDcopAlgorithm {
//  val schedule = new ParallelRandomAdjustmentSchedule(changeProbability)
//  val rule = new SimulatedAnnealingDecisionRule with NashEquilibriumConvergence with RankWeightedTargetFunction { def const = const; def k = k }
//  override def toString = "RankedDsaBVertexColoringChangeProbability" + changeProbability + "const" + const + "k" + k
//}
//
//case class RankedConflictDsanVertexColoring(changeProbability: Double, const: Double, k: Double)
//  extends RankedDcopAlgorithm {
//  val schedule = new ParallelRandomAdjustmentSchedule(changeProbability)
//  val rule = new SimulatedAnnealingDecisionRule with ZeroConflictConvergence with RankWeightedTargetFunction { def const = const; def k = k }
//  override def toString = "RankedConflictDsaBVertexColoringChangeProbability" + changeProbability + "const" + const + "k" + k
//}
//
//case class NoRankConflictDsanVertexColoring(changeProbability: Double, const: Double, k: Double)
//  extends RankedDcopAlgorithm with TargetFunctionsWithUtilityFunctions[Int, Int] { //the TargetFunctionsWithUtilityFunctions enables to add with MemoryLessTargetFunction and ConflictBasedVertexColoringUtility
//  val schedule = new ParallelRandomAdjustmentSchedule(changeProbability)
//  val rule = new SimulatedAnnealingDecisionRule with ZeroConflictConvergence with MemoryLessTargetFunction with ConflictBasedVertexColoringUtility { def const = const; def k = k }
//  override def toString = "NoRankConflictDsaBVertexColoringChangeProbability" + changeProbability + "const" + const + "k" + k
//}
//
//case class DynamicRankedConflictDsanVertexColoring(changeProbability: Double, const: Double, k: Double)
//  extends RankedDcopAlgorithm {
//  val schedule = new ParallelRandomAdjustmentSchedule(changeProbability)
//  val rule = new SimulatedAnnealingDecisionRule with ZeroConflictConvergence with DynamicRankWeightedTargetFunction with ConflictBasedVertexColoringUtility { def const = const; def k = k }
//  override def toString = "DynamicRankedConflictDsaBVertexColoringChangeProbability" + changeProbability + "const" + const + "k" + k
//}
//
///**
// * RankDependentInertia
// */
//
//case class RankedConflictDsanVertexColoringWithRankedChangeProbability(relativeChangeProbability: Double, const: Double, k: Double)
//  extends RankedDcopAlgorithm with RankedAdjustmentSchedules[Int, Int] {
//  val schedule = new RankedBasedAdjustmentSchedule(relativeChangeProbability)
//  val rule = new SimulatedAnnealingDecisionRule with ZeroConflictConvergence with RankWeightedTargetFunction { def const = const; def k = k }
//  override def toString = "RankedConflictDsaBVertexColoringWithRankedChangeProbability" + relativeChangeProbability + "const" + const + "k" + k
//}
//
//case class NoRankConflictDsanVertexColoringWithRankedChangeProbability(relativeChangeProbability: Double, const: Double, k: Double)
//  extends RankedDcopAlgorithm with TargetFunctionsWithUtilityFunctions[Int, Int] with RankedAdjustmentSchedules[Int, Int] { //the TargetFunctionsWithUtilityFunctions enables to add with MemoryLessTargetFunction and ConflictBasedVertexColoringUtility
//  val schedule = new RankedBasedAdjustmentSchedule(relativeChangeProbability)
//  val rule = new SimulatedAnnealingDecisionRule with ZeroConflictConvergence with MemoryLessTargetFunction with ConflictBasedVertexColoringUtility { def const = const; def k = k }
//  override def toString = "NoRankConflictDsaBVertexColoringWithRankedChangeProbability" + relativeChangeProbability + "const" + const + "k" + k
//}
//
//case class RankedConflictDsanVertexColoringWithInvertedRankedChangeProbability(relativeChangeProbability: Double, const: Double, k: Double)
//  extends RankedDcopAlgorithm with RankedAdjustmentSchedules[Int, Int] {
//  val schedule = new InvertRankedBasedAdjustmentSchedule(relativeChangeProbability)
//  val rule = new SimulatedAnnealingDecisionRule with ZeroConflictConvergence with RankWeightedTargetFunction { def const = const; def k = k }
//  override def toString = "RankedConflictDsaBVertexColoringWithInvertedRankedChangeProbability" + relativeChangeProbability + "const" + const + "k" + k
//}
//
//case class NoRankConflictDsanVertexColoringWithInvertedRankedChangeProbability(relativeChangeProbability: Double, const: Double, k: Double)
//  extends RankedDcopAlgorithm with TargetFunctionsWithUtilityFunctions[Int, Int] with RankedAdjustmentSchedules[Int, Int] { //the TargetFunctionsWithUtilityFunctions enables to add with MemoryLessTargetFunction and ConflictBasedVertexColoringUtility
//  val schedule = new InvertRankedBasedAdjustmentSchedule(relativeChangeProbability)
//  val rule = new SimulatedAnnealingDecisionRule with ZeroConflictConvergence with MemoryLessTargetFunction with ConflictBasedVertexColoringUtility { def const = const; def k = k }
//  override def toString = "NoRankConflictDsaBVertexColoringWithInvertedRankedChangeProbability" + relativeChangeProbability + "const" + const + "k" + k
//}
//
