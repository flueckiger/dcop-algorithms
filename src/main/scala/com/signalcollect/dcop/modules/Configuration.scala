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

