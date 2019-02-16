package com.crossman

import java.util.UUID

import akka.actor.Scheduler
import akka.typed.scaladsl.Actor
import akka.typed.{ActorRef, ActorSystem, Behavior}
import akka.util.Timeout
import com.crossman.Building.{GetFloor, MoveElevator}

import scala.collection.mutable.{Map => MMap}
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

case class Building private(numElevators: Int, numFloors: Int, actor: ActorRef[Building.Message])(implicit system: ActorSystem[Nothing]) {
	import akka.typed.scaladsl.AskPattern._
	implicit val ec: ExecutionContext = system.executionContext
	implicit val scheduler: Scheduler = system.scheduler

	def getFloor(id: Elevator.Id)(implicit timeout: Timeout): Future[Floor.Id] = {
		actor ? (GetFloor(id, _))
	}

	def moveElevatorToFloor(elevator: Elevator.Id, floor: Floor.Id)(implicit timeout: Timeout): Future[Boolean] = {
		actor ? (MoveElevator(elevator,floor,_))
	}

}

object Building {
	sealed trait Message
	final case class GetFloor(elevator: Elevator.Id, replyTo: ActorRef[Floor.Id]) extends Message
	final case class MoveElevator(elevator: Elevator.Id, floor: Floor.Id, replyTo: ActorRef[Boolean]) extends Message

	private def initialize(numElevators: Int, numFloors: Int)(implicit system: ActorSystem[Nothing], timeout: Timeout): Behavior[Message] = {
		val elevators = MMap.empty[Elevator.Id,Elevator]
		val floors = MMap.empty[Elevator.Id,Floor.Id]
		val legalFloors = scala.collection.mutable.Set.empty[Floor.Id]
		(0 to numFloors).foreach(i => {
			legalFloors += Floor.Id(i)
		})
		(0 to numElevators).foreach(i => {
			val id = Elevator.Id(i)
			val elevator: Elevator = Await.result(Elevator.create(id),Duration.Inf)
			elevators.put(id,elevator)
			floors.put(id,Floor.Id(0))
		})
		listening(elevators.toMap,floors.toMap,legalFloors.toSet)
	}

	private def listening(elevators: Map[Elevator.Id,Elevator], floors: Map[Elevator.Id,Floor.Id], legalFloors: Set[Floor.Id]): Behavior[Message] = {
		Actor.immutablePartial {
			case (_, GetFloor(id,replyTo)) =>
				replyTo ! floors(id)
				Actor.same

			case (_, MoveElevator(elevator,floor,replyTo)) =>
				if (legalFloors(floor) && floors(elevator) != floor) {
					replyTo ! true
					listening(elevators, floors + ((elevator, floor)), legalFloors)
				} else {
					replyTo ! false
					Actor.same
				}
		}
	}

	def create(numElevators: Int, numFloors: Int)(implicit system: ActorSystem[Nothing], timeout: Timeout): Future[Building] = {
		implicit val ec: ExecutionContext = system.executionContext
		val id = UUID.randomUUID()
		val fut = system.systemActorOf[Message](initialize(numElevators,numFloors), "building$" + id)
		fut.map { actor =>
			Building(numElevators,numFloors,actor)
		}
	}
}
