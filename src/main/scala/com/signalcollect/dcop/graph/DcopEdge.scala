package com.signalcollect.dcop.graph

import com.signalcollect.DefaultEdge
import com.signalcollect.Vertex
import com.signalcollect.dcop.modules.Configuration

class DcopEdge[Id](targetId: Id) extends DefaultEdge(targetId) {
  type Source = Vertex[_, _ <: Configuration[Id, _, _], _, _]

  override def signal = source.state.centralVariableValue
}
