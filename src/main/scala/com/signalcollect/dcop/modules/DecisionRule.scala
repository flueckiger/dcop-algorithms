package com.signalcollect.dcop.modules

trait DecisionRule[AgentId, Action, Config <: Configuration[AgentId, Action, Config], UtilityType] extends Serializable with TargetFunction[AgentId, Action, Config, UtilityType] {
  implicit protected def utilEv: Ordering[UtilityType]

  def computeMove(c: Config): Action
  def shouldTerminate(c: Config): Boolean
  
  def isInLocalOptimum(c: Config): Boolean = {
    val expectedUtilities: Map[Action, UtilityType] = computeExpectedUtilities(c)
    val maxUtility = expectedUtilities.values.max
    isInLocalOptimumGivenUtilitiesAndMaxUtility(c, expectedUtilities, maxUtility)
  }

  protected final def isInLocalOptimumGivenUtilitiesAndMaxUtility(
    c: Config, 
    expectedUtilities: Map[Action, UtilityType], 
    maxUtility: UtilityType): Boolean = {
    val currentUtility = expectedUtilities(c.centralVariableValue)
    maxUtility == currentUtility
  }
      
}
  
