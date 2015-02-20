package com.signalcollect.dcop.impl

import com.signalcollect.dcop.modules._

case class DefaultSimpleConfig[Id, Action, UtilityType](
  val neighborhood: Map[Id, Action],
  val numberOfCollects: Long,
  val domain: Set[Action],
  val centralVariableAssignment: (Id, Action),
  val utilityType: UtilityType = 0) extends SimpleConfig[Id, Action, UtilityType, DefaultSimpleConfig[Id, Action, UtilityType]] {
  final def withCentralVariableAssignment(value: Action) = {
    this.copy(centralVariableAssignment = (centralVariableAssignment._1, value))
  }

  //TODO: Used for ArgmaxB decision rule and for ZeroConflictConvergenceDetection.
  def computeExpectedNumberOfConflicts = {
    val occupiedColors = neighborhood.values
    val numberOfConflicts = occupiedColors.filter(_ == centralVariableValue).size
    numberOfConflicts
  }

  override def collect(neighborhood: Map[Id, Action]) =
    copy(neighborhood = neighborhood, numberOfCollects = numberOfCollects + 1)

  override def toString = s"      neighborhood = $neighborhood.toString\n" +
    s"      domain = $domain.toString\n" +
    s"      centralVariableAssignment = $centralVariableAssignment.toString\n"
}


case class DefaultMemoryConfig[Id, Action, UtilityType](
  val neighborhood: Map[Id, Action],
  val memory: Map[Action, UtilityType],
  val numberOfCollects: Long,
  val domain: Set[Action],
  val centralVariableAssignment: (Id, Action)) extends MemoryConfig[Id, Action, UtilityType, DefaultMemoryConfig[Id, Action, UtilityType]] {
  
  final def withCentralVariableAssignment(value: Action) = {
    this.copy(centralVariableAssignment = (centralVariableAssignment._1, value))
  }

  def computeExpectedNumberOfConflicts = {
    val occupiedColors = neighborhood.values
    val numberOfConflicts = occupiedColors.filter(_ == centralVariableValue).size
    numberOfConflicts
  }

  override def collect(neighborhood: Map[Id, Action]) =
    copy(neighborhood = neighborhood)

  override def collect(neighborhood: Map[Id, Action], memory: Map[Action, UtilityType]) =
    copy(neighborhood = neighborhood, memory = memory, numberOfCollects = numberOfCollects + 1)

  override def toString = s"\n      neighborhood = $neighborhood.toString\n" +
    s"      domain = $domain.toString\n" +
    s"      centralVariableAssignment = $centralVariableAssignment.toString\n" +
    s"      memory = $memory\n"
}


case class DefaultRankedConfig[Id, Action, UtilityType](
  val neighborhood: Map[Id, Action],
  val numberOfCollects: Long,
  val ranks: Map[Id, UtilityType],
  val domain: Set[Action],
  val centralVariableAssignment: (Id, Action)) extends RankedConfig[Id, Action, UtilityType, DefaultRankedConfig[Id, Action, UtilityType]] {

  /*
   * Not only the value changes, but also the rank.  
   */
  final def withCentralVariableAssignment(value: Action) = {
    this.copy(centralVariableAssignment = (centralVariableAssignment._1, value))
  }

  def computeExpectedNumberOfConflicts = {
    val occupiedColors = neighborhood.values
    val numberOfConflicts = occupiedColors.filter(_ == centralVariableValue).size
    numberOfConflicts
  }

  override def collect(neighborhood: Map[Id, Action], ranks: Map[Id, UtilityType]) =
    copy(neighborhood = neighborhood, ranks = ranks)

  override def collect(ranks: Map[Id, UtilityType]) =
    copy(ranks = ranks, numberOfCollects = numberOfCollects + 1)

  override def toString = s"      neighborhood = $neighborhood.toString\n" +
    s"      ranks = $ranks.toString\n" +
    s"      domain = $domain.toString\n" +
    s"      centralVariableAssignment = $centralVariableAssignment.toString\n"

}

