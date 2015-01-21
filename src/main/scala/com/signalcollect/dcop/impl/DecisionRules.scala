package com.signalcollect.dcop.impl

import scala.util.Random
import com.signalcollect.dcop.modules._


/**
 * "If the agent's state at t-1 is one of the states that maximizes the target function, 
 * then it is the state selected at t. Otherwise, a new state is randomly selected from the set of target function
 * maximizing states." 
 */
trait ArgmaxADecisionRule[AgentId, Action, Config <: Configuration[AgentId, Action, Config]] extends DecisionRule[AgentId, Action, Config] with TargetFunction[AgentId, Action, Config, Double] {

  def computeMove(c: Config) = {
    val expectedUtilities: Map[Action, Double] = computeExpectedUtilities(c)
    val maxUtility = expectedUtilities.values.max
    if (isInLocalOptimumGivenUtilitiesAndMaxUtility(c, expectedUtilities, maxUtility)) {
      c.centralVariableValue 
    } else {
      val maxUtilityMoves: Seq[Action] = expectedUtilities.filter(_._2 == maxUtility).map(_._1).toSeq
      if (maxUtilityMoves.size <= 0)println(expectedUtilities)
      assert(maxUtilityMoves.size > 0)
      val chosenMaxUtilityMove = maxUtilityMoves(Random.nextInt(maxUtilityMoves.size))
      chosenMaxUtilityMove
    }
  }

}

trait ArgmaxBDecisionRule[AgentId, Action, Config <: Configuration[AgentId, Action, Config]] extends ArgmaxADecisionRule[AgentId, Action, Config] {

  //TODO: Rewrite conditions for computeMove like in the Exploration version
  override def computeMove(c: Config) = {
    val expectedUtilities: Map[Action, Double] = computeExpectedUtilities(c)
    val maxUtility = expectedUtilities.values.max
    val maxUtilityMoves: Seq[Action] = expectedUtilities.filter(_._2 == maxUtility).map(_._1).toSeq
    val numberOfMaxUtilityMoves = maxUtilityMoves.size

    //If we are converged already don't stir the boat
    // Attention! If isConverged no longer depends on the utility so 
    // the maxUtility move may not be the current move anymore...
    if ((isInLocalOptimumGivenUtilitiesAndMaxUtility(c, expectedUtilities, maxUtility)) &&
      (maxUtilityMoves.contains(c.centralVariableValue)) &&
      (c.computeExpectedNumberOfConflicts == 0)) {
      c.centralVariableValue
    } else {
      val chosenMaxUtilityMove = maxUtilityMoves(Random.nextInt(maxUtilityMoves.size))
      chosenMaxUtilityMove
    }
  }
}

trait ExplorerArgmaxBDecisionRule[AgentId, Action, Config <: Configuration[AgentId, Action, Config]] extends ArgmaxADecisionRule[AgentId, Action, Config] {

  def expl: Double

  override def computeMove(c: Config) = {
    val expectedUtilities: Map[Action, Double] = computeExpectedUtilities(c)
    val maxUtility = expectedUtilities.values.max
    val maxUtilityMoves: Seq[Action] = expectedUtilities.filter(_._2 == maxUtility).map(_._1).toSeq
    val numberOfMaxUtilityMoves = maxUtilityMoves.size

    def hasNoConflictsAtNashEquilibrium =
      (maxUtilityMoves.contains(c.centralVariableValue)) &&
        (c.computeExpectedNumberOfConflicts == 0)

    def hasConflictsAtNashEquilibrium =
      (maxUtilityMoves.contains(c.centralVariableValue)) &&
        (c.computeExpectedNumberOfConflicts > 0)

    if (hasNoConflictsAtNashEquilibrium) {
      c.centralVariableValue
    } else { // If there are still conflicts we might explore with very low probability
      if (hasConflictsAtNashEquilibrium && (Random.nextDouble < expl)) { //we added that maxDelta ==0
        val exploringMove = c.domain.toSeq(Random.nextInt(c.domain.size))
        exploringMove
      } else { // We randomly choose one of the solutions that give us the maximum
        val chosenMaxUtilityMove = maxUtilityMoves(Random.nextInt(maxUtilityMoves.size))
        chosenMaxUtilityMove
      }
    }
  }
}

trait SimulatedAnnealingDecisionRule[AgentId, Action, Config <: Configuration[AgentId, Action, Config]] extends ArgmaxADecisionRule[AgentId, Action, Config] {

  def const: Double
  def k: Double
  var iteration = 0

  def eta(i: Int) = const / i*i
  var deltaComp = 0.0
  
  override def computeMove(c: Config) = {
    iteration += 1
    val randomMove = c.domain.toSeq(Random.nextInt(c.domain.size))
    val expectedUtilities = computeExpectedUtilities(c).toMap[Action, Double]
    val delta = expectedUtilities.getOrElse[Double](randomMove, -1) - expectedUtilities.getOrElse[Double](c.centralVariableValue, -1)
    deltaComp = delta
    val probab = if (delta == 0) 0.001 else scala.math.exp( delta * iteration* iteration / 1000 ) //delta / eta(iteration))
    if (delta > 0 || (delta <= 0 && Random.nextDouble < probab)) {
      randomMove
    } else {
      c.centralVariableValue
    }
  }
  
 // override def shouldTerminate(c: Config): Boolean = isInLocalOptimum(c)&&(scala.math.exp( deltaComp *iteration*iteration) < 0.01)
}

trait LinearProbabilisticDecisionRule[AgentId, Action, Config <: Configuration[AgentId, Action, Config]] extends ArgmaxADecisionRule[AgentId, Action, Config] {


  /*
   * In the case where we have a flat distribution and normFactor would be 0, the function should return the first action. 
   */
  override def computeMove(c: Config): Action = {
    val expectedUtilities: Map[Action, Double] = computeExpectedUtilities(c)
    val normFactor = expectedUtilities.values.sum
    val selectionProb = Random.nextDouble

    var partialSum: Double = 0.0
    for (action <- expectedUtilities.keys) {
      partialSum += expectedUtilities(action)
      if (selectionProb * normFactor <= partialSum) {
        return action
      }
    }
    throw new Exception("This code should be unreachable.")
  }

}
