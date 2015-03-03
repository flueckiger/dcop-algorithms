package com.signalcollect.dcop.impl

import scala.math.Numeric.Implicits._
import scala.util.Random
import com.signalcollect.dcop.modules._


/**
 * "If the agent's state at t-1 is one of the states that maximizes the target function, 
 * then it is the state selected at t. Otherwise, a new state is randomly selected from the set of target function
 * maximizing states." 
 */
trait ArgmaxADecisionRule[AgentId, Action, Config <: Configuration[AgentId, Action, Config], UtilityType] extends DecisionRule[AgentId, Action, Config, UtilityType] with TargetFunction[AgentId, Action, Config, UtilityType] {

  def computeMove(c: Config) = {
    val expectedUtilities: Map[Action, UtilityType] = computeExpectedUtilities(c)
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

trait ArgmaxBDecisionRule[AgentId, Action, Config <: Configuration[AgentId, Action, Config], UtilityType] extends ArgmaxADecisionRule[AgentId, Action, Config, UtilityType] {

  //TODO: Rewrite conditions for computeMove like in the Exploration version
  override def computeMove(c: Config) = {
    val expectedUtilities: Map[Action, UtilityType] = computeExpectedUtilities(c)
    val maxUtility = expectedUtilities.values.max
    val maxUtilityMoves: Seq[Action] = expectedUtilities.filter(_._2 == maxUtility).map(_._1).toSeq
    val numberOfMaxUtilityMoves = maxUtilityMoves.size

    //If we are converged already don't stir the boat
    // Attention! If isConverged no longer depends on the utility so 
    // the maxUtility move may not be the current move anymore...
    if ((isInLocalOptimumGivenUtilitiesAndMaxUtility(c, expectedUtilities, maxUtility)) &&
      (maxUtilityMoves.contains(c.centralVariableValue)) &&
      (c.expectedConflicts.isEmpty)) {
      c.centralVariableValue
    } else {
      val chosenMaxUtilityMove = maxUtilityMoves(Random.nextInt(maxUtilityMoves.size))
      chosenMaxUtilityMove
    }
  }
}

trait ExplorerArgmaxBDecisionRule[AgentId, Action, Config <: Configuration[AgentId, Action, Config], UtilityType] extends ArgmaxADecisionRule[AgentId, Action, Config, UtilityType] {

  def expl: Double

  override def computeMove(c: Config) = {
    val expectedUtilities: Map[Action, UtilityType] = computeExpectedUtilities(c)
    val maxUtility = expectedUtilities.values.max
    val maxUtilityMoves: Seq[Action] = expectedUtilities.filter(_._2 == maxUtility).map(_._1).toSeq
    val numberOfMaxUtilityMoves = maxUtilityMoves.size

    def hasNoConflictsAtNashEquilibrium =
      (maxUtilityMoves.contains(c.centralVariableValue)) &&
        (c.expectedConflicts.isEmpty)

    def hasConflictsAtNashEquilibrium =
      (maxUtilityMoves.contains(c.centralVariableValue)) &&
        (c.expectedConflicts.nonEmpty)

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

trait SimulatedAnnealingDecisionRule[AgentId, Action, Config <: Configuration[AgentId, Action, Config], UtilityType] extends ArgmaxADecisionRule[AgentId, Action, Config, UtilityType] {
  implicit protected def utilEv: Numeric[UtilityType]

  def const: UtilityType
  def k: UtilityType
  var iteration = 0

  def eta(i: Int) = const.toDouble / i*i
  var deltaComp = 0.0
  
  override def computeMove(c: Config) = {
    iteration += 1
    val randomMove = c.domain.toSeq(Random.nextInt(c.domain.size))
    val expectedUtilities = computeExpectedUtilities(c).toMap[Action, UtilityType]
    val delta = (expectedUtilities.getOrElse[UtilityType](randomMove, utilEv.fromInt(-1)) - expectedUtilities.getOrElse[UtilityType](c.centralVariableValue, utilEv.fromInt(-1))).toDouble
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

trait LinearProbabilisticDecisionRule[AgentId, Action, Config <: Configuration[AgentId, Action, Config], UtilityType] extends ArgmaxADecisionRule[AgentId, Action, Config, UtilityType] {
  implicit protected def utilEv: Numeric[UtilityType]

  /*
   * In the case where we have a flat distribution and normFactor would be 0, the function should return the first action. 
   */
  override def computeMove(c: Config): Action = {
    val expectedUtilities: Map[Action, UtilityType] = computeExpectedUtilities(c)
    val normFactor = expectedUtilities.values.sum.toDouble
    val selectionProb = Random.nextDouble

    var partialSum: UtilityType = utilEv.zero
    for (action <- expectedUtilities.keys) {
      partialSum += expectedUtilities(action)
      if (selectionProb * normFactor <= partialSum.toDouble) {
        return action
      }
    }
    throw new Exception("This code should be unreachable.")
  }

}
