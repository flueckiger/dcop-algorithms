package com.signalcollect.dcop.impl

import com.signalcollect.dcop.modules._

trait VertexColoringUtility[AgentId, Action, Config <: Configuration[AgentId, Action, Config], UtilityType] extends UtilityFunction[AgentId, Action, Config, UtilityType] {
  override def computeUtilities(c: Config, a: Action) =
    c.neighborhood.transform((_, x) => if (x == a) utilEv.zero else utilEv.one)

  override def utilityBounds(c: Config) = {
    val x = (utilEv.zero, utilEv.one)
    c.neighborhood.mapValues(_ => x)
  }
}

trait ConflictBasedVertexColoringUtility[AgentId, Action, Config <: Configuration[AgentId, Action, Config], UtilityType] extends UtilityFunction[AgentId, Action, Config, UtilityType] {
  override def computeUtilities(c: Config, a: Action) =
    c.neighborhood.transform((_, x) => if (x == a) utilEv.fromInt(-1) else utilEv.zero)

  override def utilityBounds(c: Config) = {
    val x = (utilEv.fromInt(-1), utilEv.zero)
    c.neighborhood.mapValues(_ => x)
  }
}
