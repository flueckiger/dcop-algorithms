package com.signalcollect.dcop.impl

import com.signalcollect.dcop.modules._
import scala.util.Random

trait MemoryLessTargetFunction[AgentId, Action, Config <: Configuration[AgentId, Action, Config], UtilityType] extends TargetFunction[AgentId, Action, Config, UtilityType] with UtilityFunction[AgentId, Action, Config, UtilityType] {
  override def computeExpectedUtilities(c: Config) =
    c.domain.view.map(x => (x, computeUtility(c, x))).toMap
}

/**
 * MemoryTargetFunctions
 */

trait DiscountedExpectedUtilityTargetFunction[AgentId, Action, UtilityType] extends MemoryLessTargetFunction[AgentId, Action, SimpleMemoryConfig[AgentId, Action, UtilityType], UtilityType] {
  import scala.math.Numeric.Implicits._

  implicit protected def utilEv: Numeric[UtilityType]
  def rho: UtilityType
  
  override def computeExpectedUtilities(conf: SimpleMemoryConfig[AgentId, Action, UtilityType]) = {
    val configUtilities = conf.domain.view.map(x =>
      (x, if (conf.numberOfCollects > 1) rho*computeUtility(conf, x)+(utilEv.one-rho)*conf.memory(x) else computeUtility(conf, x))).toMap
    configUtilities
  }
}


trait AverageExpectedUtilityTargetFunction[AgentId, Action, UtilityType] extends MemoryLessTargetFunction[AgentId, Action, SimpleMemoryConfig[AgentId, Action, UtilityType], UtilityType] {
  import scala.math.Fractional.Implicits._

  implicit protected def utilEv: Fractional[UtilityType]
  
  override def computeExpectedUtilities(conf: SimpleMemoryConfig[AgentId, Action, UtilityType]) = {
    val configUtilities = conf.domain.view.map(x =>
      (x,
        if (conf.numberOfCollects == 0) computeUtility(conf, x) else {
          val numberOfCollects = utilEv.fromInt(((conf.numberOfCollects min Int.MaxValue) max 1).toInt)
          (computeUtility(conf, x)+(numberOfCollects - utilEv.one)*conf.memory(x))/numberOfCollects
        })
      ).toMap
    configUtilities
  }
}


trait DiscountedAverageRegretsTargetFunction[AgentId, Action, UtilityType] extends MemoryLessTargetFunction[AgentId, Action, SimpleMemoryConfig[AgentId, Action, UtilityType], UtilityType] {
  import scala.math.Numeric.Implicits._

  implicit protected def utilEv: Numeric[UtilityType]
  def rho: UtilityType

  /*
   * All regrets are minimum 0.
   */
  override def computeExpectedUtilities(conf: SimpleMemoryConfig[AgentId, Action, UtilityType]) = {
    val configUtilities = conf.domain.view.map(x =>
      (x, rho*(utilEv.max(computeUtility(conf, x) - computeUtility(conf), utilEv.zero)) + (utilEv.one- rho)*conf.memory(x))).toMap
    configUtilities
  }
}


/**
 * RankedTargetFunctions
 */

//TODO: Push the Utility calculation into UtilityFunctions.scala and replace the Double in the TargetFunction with the UtilityType.
trait RankWeightedTargetFunction[AgentId, Action, UtilityType] extends MemoryLessTargetFunction[AgentId, Action, RankedConfig[AgentId, Action, UtilityType], UtilityType] {
  import scala.math.Numeric.Implicits._

  implicit protected def utilEv: Numeric[UtilityType]

  override def computeExpectedUtilities(c: RankedConfig[AgentId, Action, UtilityType]) = {
    val configUtilities = c.domain.view.map(x => {
      val (allies, opponents) = c.neighborhood.partition(_._2 != x)
      val allyRanks = allies.keys.map(c.ranks(_)).sum
      val opponentRanks = opponents.keys.map(c.ranks(_)).sum
      val expectedUtility = allyRanks - opponentRanks
      val expectedMoveUtility = (x, expectedUtility)
      //if (configuration.centralVariableAssignment._1 == 2) {
      //println(s"Expected move utility for agent ${configuration.centralVariableAssignment._1} and move ${expectedMoveUtility._1} is ${expectedMoveUtility._2}")
      //}
      expectedMoveUtility
    })
    configUtilities.toMap
  }

  /**
   * Same as RankWeightedTargetFunction, but when it reaches a NE it behaves like the MemoryLessTargetFunction
   */
  //TODO Push the Utility calculation into UtilityFunctions.scala and replace the Double in the RankWeightedTargetFunction with the UtilityType.
  trait DynamicRankWeightedTargetFunction[AgentId, Action, UtilityType] extends RankWeightedTargetFunction[AgentId, Action, UtilityType] {

    def isAtRankedNashEquilibrium(c: RankedConfig[AgentId, Action, UtilityType]): Boolean = {
      val expectedUtilities = computeRankedExpectedUtilities(c)
      val maxUtility = expectedUtilities.values.max
      val currentUtility = expectedUtilities(c.centralVariableValue)
      maxUtility == currentUtility
    }

    def computeRankedExpectedUtilities(c: RankedConfig[AgentId, Action, UtilityType]) = super.computeExpectedUtilities(c)

    override def computeExpectedUtilities(c: RankedConfig[AgentId, Action, UtilityType]) = {
      if (!isAtRankedNashEquilibrium(c)) {
        computeRankedExpectedUtilities(c)
      } else {
        val configUtilities = c.domain.view.map(x => (x, computeUtility(c, x))).toMap
        configUtilities
      }
    }
  }

  /**
   * Same as RankWeightedTargetFunction, but when it reaches a certain iteration it behaves like the MemoryLessTargetFunction
   */
  trait Switch1RankWeightedTargetFunction[AgentId, Action, UtilityType] extends RankWeightedTargetFunction[AgentId, Action, UtilityType] {

    var iteration = 0
    var switched = false

    def computeRankedExpectedUtilities(c: RankedConfig[AgentId, Action, UtilityType]) = super.computeExpectedUtilities(c)

    override def computeExpectedUtilities(c: RankedConfig[AgentId, Action, UtilityType]) = {
      iteration += 1
      if (switched == false && iteration > 10)
        if (Random.nextDouble < 0.2)
          switched = true

      if (!switched) {
        computeRankedExpectedUtilities(c)
      } else {
        val configUtilities = c.domain.view.map(x => (x, computeUtility(c, x))).toMap
        configUtilities
      }
    }
  }

  /**
   * Same as RankWeightedTargetFunction, but when it reaches a certain iteration it behaves like the MemoryLessTargetFunction
   */
  trait Switch2RankWeightedTargetFunction[AgentId, Action, UtilityType] extends RankWeightedTargetFunction[AgentId, Action, UtilityType] {

    var iteration = 0
    var switched = false

    def computeRankedExpectedUtilities(c: RankedConfig[AgentId, Action, UtilityType]) = super.computeExpectedUtilities(c)

    override def computeExpectedUtilities(c: RankedConfig[AgentId, Action, UtilityType]) = {
      iteration += 1
      if (switched == false && iteration > 15)
        if (Random.nextDouble < 0.2)
          switched = true

      if (!switched) {
        computeRankedExpectedUtilities(c)
      } else {
        val configUtilities = c.domain.view.map(x => (x, computeUtility(c, x))).toMap
        configUtilities
      }
    }
  }

  /**
   * Same as RankWeightedTargetFunction, but when it reaches a certain iteration it behaves like the MemoryLessTargetFunction
   */
  trait Switch3RankWeightedTargetFunction[AgentId, Action, UtilityType] extends RankWeightedTargetFunction[AgentId, Action, UtilityType] {

    var iteration = 0
    var switched = false

    def computeRankedExpectedUtilities(c: RankedConfig[AgentId, Action, UtilityType]) = super.computeExpectedUtilities(c)

    override def computeExpectedUtilities(c: RankedConfig[AgentId, Action, UtilityType]) = {
      iteration += 1
      if (switched == false && iteration > 20)
        if (Random.nextDouble < 0.2)
          switched = true

      if (!switched) {
        computeRankedExpectedUtilities(c)
      } else {
        val configUtilities = c.domain.view.map(x => (x, computeUtility(c, x))).toMap
        configUtilities
      }
    }
  }

  /**
   * Same as MemoryLessTargetFunction, but when it reaches a certain iteration it behaves like the RankWeightedTargetFunction
   */
  trait SwitchInv1RankWeightedTargetFunction[AgentId, Action, UtilityType] extends RankWeightedTargetFunction[AgentId, Action, UtilityType] {

    var iteration = 0
    var switched = false

    def computeRankedExpectedUtilities(c: RankedConfig[AgentId, Action, UtilityType]) = super.computeExpectedUtilities(c)

    override def computeExpectedUtilities(c: RankedConfig[AgentId, Action, UtilityType]) = {
      iteration += 1
      if (switched == false && iteration > 10)
        if (Random.nextDouble < 0.2)
          switched = true

      if (!switched) {
        computeRankedExpectedUtilities(c)
      } else {
        val configUtilities = c.domain.view.map(x => (x, computeUtility(c, x))).toMap
        configUtilities
      }
    }
  }

  /**
   * Same as MemoryLessTargetFunction, but when it reaches a certain iteration it behaves like the RankWeightedTargetFunction
   */
  trait SwitchInv2RankWeightedTargetFunction[AgentId, Action, UtilityType] extends RankWeightedTargetFunction[AgentId, Action, UtilityType] {

    var iteration = 0
    var switched = false

    def computeRankedExpectedUtilities(c: RankedConfig[AgentId, Action, UtilityType]) = super.computeExpectedUtilities(c)

    override def computeExpectedUtilities(c: RankedConfig[AgentId, Action, UtilityType]) = {
      iteration += 1
      if (switched == false && iteration > 15)
        if (Random.nextDouble < 0.2)
          switched = true

      if (!switched) {
        computeRankedExpectedUtilities(c)
      } else {
        val configUtilities = c.domain.view.map(x => (x, computeUtility(c, x))).toMap
        configUtilities
      }
    }
  }

  /**
   * Same as MemoryLessTargetFunction, but when it reaches a certain iteration it behaves like the RankWeightedTargetFunction
   */
  trait SwitchInv3RankWeightedTargetFunction[AgentId, Action, UtilityType] extends RankWeightedTargetFunction[AgentId, Action, UtilityType] {

    var iteration = 0
    var switched = false

    def computeRankedExpectedUtilities(c: RankedConfig[AgentId, Action, UtilityType]) = super.computeExpectedUtilities(c)

    override def computeExpectedUtilities(c: RankedConfig[AgentId, Action, UtilityType]) = {
      iteration += 1
      if (switched == false && iteration > 20)
        if (Random.nextDouble < 0.2)
          switched = true

      if (!switched) {
        computeRankedExpectedUtilities(c)
      } else {
        val configUtilities = c.domain.view.map(x => (x, computeUtility(c, x))).toMap
        configUtilities
      }
    }
  }

}
