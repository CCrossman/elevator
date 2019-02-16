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
		(actor ? (GetFloor(elevator, _))) map { oldFloor: Floor.Id => {
			actor ! MoveElevator(elevator,floor)
			oldFloor != floor
		}}
	}

}

object Building {
	sealed trait Message
	final case class GetFloor(elevator: Elevator.Id, replyTo: ActorRef[Floor.Id]) extends Message
	final case class MoveElevator(elevator: Elevator.Id, floor: Floor.Id) extends Message

	private def initialize(numElevators: Int, numFloors: Int)(implicit system: ActorSystem[Nothing], timeout: Timeout): Behavior[Message] = {
		val elevators = MMap.empty[Elevator.Id,Elevator]
		val floors = MMap.empty[Elevator.Id,Floor.Id]
		(0 to numElevators).foreach(i => {
			val id = Elevator.Id(i)
			val elevator: Elevator = Await.result(Elevator.create(id),Duration.Inf)
			elevators.put(id,elevator)
			floors.put(id,Floor.Id(0))
		})
		listening(elevators.toMap,floors.toMap)
	}

	private def listening(elevators: Map[Elevator.Id,Elevator], floors: Map[Elevator.Id,Floor.Id]): Behavior[Message] = {
		Actor.immutablePartial {
			case (_, GetFloor(id,replyTo)) =>
				replyTo ! floors(id)
				Actor.same

			case (_, MoveElevator(elevator,floor)) =>
				listening(elevators,floors + ((elevator,floor)))
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
