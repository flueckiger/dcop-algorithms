package com.signalcollect.dcop.impl

import scala.util.Random
import com.signalcollect.dcop.modules._

trait AdjustmentSchedules[AgentId, Action] extends AdjustmentScheduleModule[AgentId, Action] {
  this: ConfigurationModule[AgentId, Action] =>

  class ParallelRandomAdjustmentSchedule(changeProbability: Double) extends AdjustmentSchedule {
    def shouldConsiderMove(c: Config) = {
      Random.nextDouble <= changeProbability
    }
  }

  class FloodAdjustmentSchedule extends AdjustmentSchedule {
    def shouldConsiderMove(c: Config) = true
  }

}

trait RankedAdjustmentSchedules[AgentId, Action] extends AdjustmentScheduleModule[AgentId, Action] {
  this: UtilityFunctionModule[AgentId, Action] with RankedConfiguration[AgentId, Action] =>

  class RankedBasedAdjustmentSchedule(relativeChangeProbability: Double) extends AdjustmentSchedule {
    def shouldConsiderMove(c: Config) = {
      val maxNeighbourRank = c.ranks.values.max
      val rankForCurrentConfig = c.ranks(c.centralVariableAssignment._1)
      val relativeRankRatio = rankForCurrentConfig / maxNeighbourRank
      val changeProbability = 1 - relativeRankRatio * relativeChangeProbability // The higher the rank ratio, the lower the probability to change.
      Random.nextDouble <= changeProbability
    }
  }

  class InvertRankedBasedAdjustmentSchedule(relativeChangeProbability: Double) extends AdjustmentSchedule {
    def shouldConsiderMove(c: Config) = {
      val maxNeighbourRank = c.ranks.values.max
      val rankForCurrentConfig = c.ranks(c.centralVariableAssignment._1)
      val relativeRankRatio = rankForCurrentConfig / maxNeighbourRank
      val changeProbability = relativeRankRatio * relativeChangeProbability // The higher the rank ratio, the higher the probability to change.
      Random.nextDouble <= changeProbability
    }
  }

  class DynamicRankedBasedAdjustmentSchedule(relativeChangeProbability: Double) extends AdjustmentSchedule {
    this: UtilityFunction =>

    def isAtRankedNashEquilibriumInAdjustmentSchedule(c: Config): Boolean = {
      val expectedUtilities = computeRankedExpectedUtilitiesInAdjustmentSchedule(c)
      val maxUtility = expectedUtilities.values.max
      val currentUtility = expectedUtilities(c.centralVariableValue)
      maxUtility == currentUtility
    }

    def computeRankedExpectedUtilitiesInAdjustmentSchedule(c: Config) = {
      val configurationCandidates: Set[Config] = for {
        assignment <- c.domain
      } yield c.withCentralVariableAssignment(assignment)
      val configUtilities = configurationCandidates.map(configuration => {
        val (allies, opponents) = configuration.neighborhood.partition(_._2 != configuration.centralVariableValue)
        val allyRanks = allies.keys.map(c.ranks(_)).sum
        val opponentRanks = opponents.keys.map(c.ranks(_)).sum
        val expectedUtility = allyRanks - opponentRanks
        val expectedMoveUtility = (configuration.centralVariableValue, expectedUtility)
        expectedMoveUtility
      })
      configUtilities.toMap
    }

    def shouldConsiderMove(c: Config) = {
      val maxNeighbourRank = c.ranks.values.max
      val rankForCurrentConfig = c.ranks(c.centralVariableAssignment._1)
      val relativeRankRatio = rankForCurrentConfig / maxNeighbourRank
      val changeProbability = (if (!isAtRankedNashEquilibriumInAdjustmentSchedule(c)) 1 - relativeRankRatio else relativeRankRatio) * relativeChangeProbability // The higher the rank ratio, the higher the probability to change.
      Random.nextDouble <= changeProbability
    }
  }

  //TODO: Finish this and test
  //  class DiscountedRankedBasedAdjustmentSchedule(baseChangeProbability: Double) extends AdjustmentSchedule {
  //    var stepCount: Int = 0 
  //    
  //    def shouldConsiderMove(c: Config) = {
  //      val maxNeighbourRank = c.ranks.values.max
  //      val rankForCurrentConfig = c.ranks(c.centralVariableAssignment._1)
  //      val relativeRankRatio = rankForCurrentConfig / maxNeighbourRank // Ranks are > 0
  //      val changeProbability = baseChangeProbability - relativeRankRatio * relativeChangeProbability // The higher the rank ratio, the lower the probability to change.
  //      Random.nextDouble <= changeProbability
  //    }
  //  }

}

