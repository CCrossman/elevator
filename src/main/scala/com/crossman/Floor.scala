package com.crossman

class Floor(id: Floor.Id) {
	private val _elevators = scala.collection.mutable.Set.empty[Elevator.Id]

	def elevators: Set[Elevator.Id] = _elevators.toSet

	def enterFloor(id: Elevator.Id): Unit = {
		_elevators += id
	}

	def leaveFloor(id: Elevator.Id): Unit = {
		_elevators -= id
	}
}

object Floor {
	final case class Id(value: Int) extends AnyVal
	
	sealed trait Message

	def create(id: Id): Floor = {
		new Floor(id)
	}
}
