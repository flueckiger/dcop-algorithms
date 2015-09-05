package com.signalcollect.dcop.modules


trait UtilityFunction[AgentId, Action, Config <: Configuration[AgentId, Action, Config], UtilityType] extends Serializable {
  implicit protected def utilEv: Numeric[UtilityType]

  def computeUtilities(c: Config, a: Action): Map[AgentId, UtilityType]

  def computeUtilities(c: Config): Map[AgentId, UtilityType] = computeUtilities(c, c.centralVariableValue)

  def computeUtility(c: Config, a: Action): UtilityType = computeUtilities(c, a).values.sum

  def computeUtility(c: Config): UtilityType = computeUtilities(c).values.sum

  def utilityBounds(c: Config): Map[AgentId, (UtilityType, UtilityType)]

  def utilityBound(c: Config): (UtilityType, UtilityType) = {
    val x = utilityBounds(c).values
    (x.map(_._1).min, x.map(_._2).max)
  }
}
