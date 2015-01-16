/*
 *  @author Philip Stutz
 *  @author Mihaela Verman
 *  
 *  Copyright 2013 University of Zurich
 *      
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *  
 *         http://www.apache.org/licenses/LICENSE-2.0
 *  
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *  
 */

package com.signalcollect.dcop

import scala.util.Random
import org.scalatest.FlatSpec
import org.scalatest.ShouldMatchers
import org.scalatest.prop.Checkers
import com.signalcollect._
import com.signalcollect.dcop.graph._
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import com.signalcollect.dcop.optimizers._
import com.signalcollect.dcop.impl._

class ModulesSpec extends FlatSpec with ShouldMatchers with Checkers with TestAnnouncements {

    "A 2-Vertex graph" should "correctly assign 2-colors with Dsan" in {
    check(
      {
        def initial0Value = 0

        for (i <- (1 to 50)) {
          val g = GraphBuilder.build
          try {
            def initialConf(id: Int) = SimpleConfig[Int, Int](
              neighborhood = Map.empty[Int, Int].withDefaultValue(0),
              numberOfCollects = 0,
              domain = Set(0, 1, 2, 3),
              centralVariableAssignment = (id, initial0Value))

            val vertex0 = new SimpleDcopVertex(initialConf(0))(new DsanVertexColoring(0.5, 1, 2), debug = false)
            val vertex1 = new SimpleDcopVertex(initialConf(1))(new DsanVertexColoring(0.5, 1, 2), debug = false)
            g.addVertex(vertex0)
            g.addVertex(vertex1)
            g.addEdge(0, new SimpleDcopEdge(1))
            g.addEdge(1, new SimpleDcopEdge(0))
            println(g.execute)
            assert(vertex0.state.centralVariableValue != vertex1.state.centralVariableValue, "Color collision")
          } finally {
            g.shutdown
          }
        }

        true
      },
      minSuccessful(1))
  }
  
  "A 2-Vertex Ranked-graph" should "correctly assign 2-colors" in {
    check(
      {
        def initial0Value = 0

        for (i <- (1 to 50)) {
          val g = GraphBuilder.build
          try {
            def initialConf(id: Int) = RankedConfig(
              neighborhood = Map.empty[Int, Int].withDefaultValue(0),
              numberOfCollects = 0,
              ranks = Map.empty[Int, Double].withDefaultValue(0),
              domain = Set(0, 1),
              centralVariableAssignment = (id, initial0Value))

            val vertex0 = new RankedDcopVertex(new RankedDsaAVertexColoring[Int, Int](0.5), initialConf(0), debug = false)
            val vertex1 = new RankedDcopVertex(new RankedDsaAVertexColoring[Int, Int](0.5), initialConf(1), debug = false)
            g.addVertex(vertex0)
            g.addVertex(vertex1)
            g.addEdge(0, new RankedDcopEdge(1))
            g.addEdge(1, new RankedDcopEdge(0))
            println(g.execute)
            assert(vertex0.state.centralVariableValue != vertex1.state.centralVariableValue, "Color collision")
            assert(math.abs(vertex0.currentConfig.ranks(vertex0.id) - vertex1.currentConfig.ranks(vertex1.id)) < 0.01, "Should have almost the same rank for both vertices.")
            assert(math.abs(vertex0.currentConfig.ranks(vertex0.id) - vertex1.currentConfig.ranks(vertex0.id)) < 0.01, "Should have the correct rank of the neighbour")
          } finally {
            g.shutdown
          }
        }
        true
      },
      minSuccessful(1))
  }

  it should "have all vertices in a local minimum" in {
    check(
      {
        def initial0Value = 0

        for (i <- (1 to 50)) {
          val g = GraphBuilder.build
          try {
            def initialConf(id: Int) = RankedConfig(
              neighborhood = Map.empty[Int, Int].withDefaultValue(0),
              numberOfCollects = 0,
              ranks = Map.empty[Int, Double].withDefaultValue(0),
              domain = Set(0, 1),
              centralVariableAssignment = (id, initial0Value))

            val vertex0 = new RankedDcopVertex(new RankedDsaAVertexColoring[Int, Int](0.5), initialConf(0), debug = false)
            val vertex1 = new RankedDcopVertex(new RankedDsaAVertexColoring[Int, Int](0.5), initialConf(1), debug = false)
            g.addVertex(vertex0)
            g.addVertex(vertex1)
            g.addEdge(0, new RankedDcopEdge(1))
            g.addEdge(1, new RankedDcopEdge(0))
            println(g.execute)
            assert(vertex0.state.centralVariableValue != vertex1.state.centralVariableValue, "Color collision")
            assert(math.abs(vertex0.currentConfig.ranks(vertex0.id) - vertex1.currentConfig.ranks(vertex1.id)) < 0.01, "Should have almost the same rank for both vertices.")
            assert(math.abs(vertex0.currentConfig.ranks(vertex0.id) - vertex1.currentConfig.ranks(vertex0.id)) < 0.01, "Should have the correct rank of the neighbour")
          } finally {
            g.shutdown
          }
        }
        true
      },
      minSuccessful(1))
  }

  "A 2-Vertex graph with memory running Fading Memory JSFP-I" should "correctly assign 2-colors" in {
    check(
      {
        def initial0Value = 0

        for (i <- (1 to 50)) {
          val g = GraphBuilder.build
          try {
            def initialConf(id: Int) = SimpleMemoryConfig[Int, Int, Double](
              neighborhood = Map.empty[Int, Int].withDefaultValue(0),
              memory = Map.empty[Int, Double].withDefaultValue(0),
              numberOfCollects = 0,
              domain = Set(0, 1),
              centralVariableAssignment = (id, initial0Value))

            val vertex0 = new MemoryDcopVertex(new FadingMemoryJsfpiVertexColoring[Int, Int](0.5, 0.7), initialConf(0), debug = false)
            val vertex1 = new MemoryDcopVertex(new FadingMemoryJsfpiVertexColoring[Int, Int](0.5, 0.7), initialConf(1), debug = false)
            g.addVertex(vertex0)
            g.addVertex(vertex1)
            g.addEdge(0, new MemoryDcopEdge(1))
            g.addEdge(1, new MemoryDcopEdge(0))
            println(g.execute)
            assert(vertex0.state.centralVariableValue != vertex1.state.centralVariableValue, "Color collision")
          } finally {
            g.shutdown
          }
        }

        true
      },
      minSuccessful(1))
  }

  "A 2-Vertex graph with memory running WRM-I" should "correctly assign 2-colors" in {
    check(
      {
        def initial0Value = 0

        for (i <- (1 to 50)) {
          val g = GraphBuilder.build
          try {

            def initialConf(id: Int) = SimpleMemoryConfig[Int, Int, Double](
              neighborhood = Map.empty[Int, Int].withDefaultValue(0),
              memory = Map.empty[Int, Double].withDefaultValue(0),
              numberOfCollects = 0,
              domain = Set(0, 1),
              centralVariableAssignment = (id, initial0Value))

            val vertex0 = new MemoryDcopVertex(new WrmiVertexColoring[Int, Int](0.5, 0.7), initialConf(0), debug = false)
            val vertex1 = new MemoryDcopVertex(new WrmiVertexColoring[Int, Int](0.5, 0.7), initialConf(1), debug = false)
            g.addVertex(vertex0)
            g.addVertex(vertex1)
            g.addEdge(0, new MemoryDcopEdge(1))
            g.addEdge(1, new MemoryDcopEdge(0))
            println(g.execute)
            assert(vertex0.state.centralVariableValue != vertex1.state.centralVariableValue, "Color collision")
          } finally {
            g.shutdown
          }
        }

        true
      },
      minSuccessful(1))
  }

  "A 2-Vertex graph" should "correctly assign 2-colors" in {
    check(
      {
        def initial0Value = 0

        for (i <- (1 to 50)) {
          val g = GraphBuilder.build
          try {
            def initialConf(id: Int) = SimpleConfig[Int, Int](
              neighborhood = Map.empty[Int, Int].withDefaultValue(0),
              numberOfCollects = 0,
              domain = Set(0, 1),
              centralVariableAssignment = (id, initial0Value))

            val vertex0 = new SimpleDcopVertex(initialConf(0))(new DsaAVertexColoring(0.5), debug = false)
            val vertex1 = new SimpleDcopVertex(initialConf(1))(new DsaAVertexColoring(0.5), debug = false)
            g.addVertex(vertex0)
            g.addVertex(vertex1)
            g.addEdge(0, new SimpleDcopEdge(1))
            g.addEdge(1, new SimpleDcopEdge(0))
            println(g.execute)
            assert(vertex0.state.centralVariableValue != vertex1.state.centralVariableValue, "Color collision")
          } finally {
            g.shutdown
          }
        }

        true
      },
      minSuccessful(1))
  }
  


}



