package com.signalcollect.dcop.modules


trait UtilityFunction[AgentId, Action, Config <: Configuration[AgentId, Action, Config], UtilityType] extends Serializable {
  def computeUtility(c: Config): UtilityType

  def computeUtility(c: Config, a: Action): UtilityType = computeUtility(c.withCentralVariableAssignment(a))
}
