package com.signalcollect.dcop.modules

trait Configuration[AgentId, Action, +Config <: Configuration[AgentId, Action, Config]] extends Serializable {
  def neighborhood: Map[AgentId, Action]
  def numberOfCollects: Long
  def domain: Set[Action]
  def withCentralVariableAssignment(value: Action): Config
  def centralVariableAssignment: (AgentId, Action)
  def centralVariableValue = centralVariableAssignment._2
  def computeExpectedNumberOfConflicts: Int
}

trait UtilityConfig[AgentId, Action, +UtilityType, +Config <: UtilityConfig[AgentId, Action, UtilityType, Config]] extends Configuration[AgentId, Action, Config] {}

trait SimpleConfig[AgentId, Action, +UtilityType, +Config <: SimpleConfig[AgentId, Action, UtilityType, Config]] extends UtilityConfig[AgentId, Action, UtilityType, Config] {
  def collect(neighborhood: Map[AgentId, Action]): Config
}

trait MemoryConfig[AgentId, Action, UtilityType, +Config <: MemoryConfig[AgentId, Action, UtilityType, Config]] extends UtilityConfig[AgentId, Action, UtilityType, Config] {
  def memory: Map[Action, UtilityType]
  def collect(neighborhood: Map[AgentId, Action]): Config
  def collect(neighborhood: Map[AgentId, Action], memory: Map[Action, UtilityType]): Config
}

trait RankedConfig[AgentId, Action, UtilityType, +Config <: RankedConfig[AgentId, Action, UtilityType, Config]] extends UtilityConfig[AgentId, Action, UtilityType, Config] {
  def ranks: Map[AgentId, UtilityType]
  def collect(neighborhood: Map[AgentId, Action], ranks: Map[AgentId, UtilityType]): Config
  def collect(ranks: Map[AgentId, UtilityType]): Config
}
