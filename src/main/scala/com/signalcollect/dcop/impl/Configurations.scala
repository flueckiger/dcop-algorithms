package com.signalcollect.dcop.impl

import com.signalcollect.dcop.modules._

case class SimpleConfig[Id, Action, UtilityType](
  val neighborhood: Map[Id, Action],
  val numberOfCollects: Long,
  val domain: Set[Action],
  val centralVariableAssignment: (Id, Action),
  val utilityType: UtilityType = 0) extends Configuration[Id, Action, SimpleConfig[Id, Action, UtilityType]] {
  final def withCentralVariableAssignment(value: Action) = {
    this.copy(centralVariableAssignment = (centralVariableAssignment._1, value))
  }

  //TODO: Used for ArgmaxB decision rule and for ZeroConflictConvergenceDetection.
  def computeExpectedNumberOfConflicts = {
    val occupiedColors = neighborhood.values
    val numberOfConflicts = occupiedColors.filter(_ == centralVariableValue).size
    numberOfConflicts
  }

  override def toString = s"      neighborhood = $neighborhood.toString\n" +
    s"      domain = $domain.toString\n" +
    s"      centralVariableAssignment = $centralVariableAssignment.toString\n"
}


case class SimpleMemoryConfig[Id, Action, UtilityType](
  val neighborhood: Map[Id, Action],
  val memory: Map[Action, UtilityType],
  val numberOfCollects: Long,
  val domain: Set[Action],
  val centralVariableAssignment: (Id, Action)) extends Configuration[Id, Action, SimpleMemoryConfig[Id, Action, UtilityType]] {
  
  final def withCentralVariableAssignment(value: Action) = {
    this.copy(centralVariableAssignment = (centralVariableAssignment._1, value))
  }

  def computeExpectedNumberOfConflicts = {
    val occupiedColors = neighborhood.values
    val numberOfConflicts = occupiedColors.filter(_ == centralVariableValue).size
    numberOfConflicts
  }

  override def toString = s"\n      neighborhood = $neighborhood.toString\n" +
    s"      domain = $domain.toString\n" +
    s"      centralVariableAssignment = $centralVariableAssignment.toString\n" +
    s"      memory = $memory\n"
}


case class RankedConfig[Id, Action, UtilityType](
  val neighborhood: Map[Id, Action],
  val numberOfCollects: Long,
  val ranks: Map[Id, UtilityType],
  val domain: Set[Action],
  val centralVariableAssignment: (Id, Action)) extends Configuration[Id, Action, RankedConfig[Id, Action, UtilityType]] {

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

  override def toString = s"      neighborhood = $neighborhood.toString\n" +
    s"      ranks = $ranks.toString\n" +
    s"      domain = $domain.toString\n" +
    s"      centralVariableAssignment = $centralVariableAssignment.toString\n"

}

