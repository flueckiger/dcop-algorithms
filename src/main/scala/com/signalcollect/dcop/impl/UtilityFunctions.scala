package com.signalcollect.dcop.impl

import com.signalcollect.dcop.modules._

trait VertexColoringUtility[AgentId, Action, Config <: Configuration[AgentId, Action, Config], UtilityType] extends UtilityFunction[AgentId, Action, Config, UtilityType] {
  implicit protected def utilEv: Numeric[UtilityType]

  def computeUtility(c: Config) = {
    val occupiedColors = c.neighborhood.values
    val numberOfConflicts = occupiedColors.filter(_ == c.centralVariableValue).size
    val numberOfNeighbors = occupiedColors.size
    val neighborsInSync = numberOfNeighbors - numberOfConflicts
    utilEv.fromInt(neighborsInSync)
  }
}

trait ConflictBasedVertexColoringUtility[AgentId, Action, Config <: Configuration[AgentId, Action, Config], UtilityType] extends UtilityFunction[AgentId, Action, Config, UtilityType] {
  implicit protected def utilEv: Numeric[UtilityType]

  def computeUtility(c: Config) = {
    val occupiedColors = c.neighborhood.values
    val numberOfConflicts = occupiedColors.filter(_ == c.centralVariableValue).size
    utilEv.fromInt(-numberOfConflicts)
  }
}
