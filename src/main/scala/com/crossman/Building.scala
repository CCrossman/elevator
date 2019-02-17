package com.crossman

import java.util.UUID

import akka.actor.Scheduler
import akka.typed.scaladsl.Actor
import akka.typed.{ActorRef, ActorSystem, Behavior}
import akka.util.Timeout
import com.crossman.Building._
import com.crossman.Elevator.Velocity

import scala.collection.mutable.{Map => MMap}
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.language.postfixOps

case class Building private(numElevators: Int, numFloors: Int, actor: ActorRef[Building.Message])(implicit system: ActorSystem[Nothing]) {
	import akka.typed.scaladsl.AskPattern._
	implicit val ec: ExecutionContext = system.executionContext
	implicit val scheduler: Scheduler = system.scheduler

	def getLocation(id: Elevator.Id)(implicit timeout: Timeout): Future[Floor.Id] = {
		actor ? (GetLocation(id, _))
	}

	def getVelocity(id: Elevator.Id)(implicit timeout: Timeout): Future[Velocity] = {
		actor ? (GetVelocity(id, _))
	}

	def moveElevatorToFloor(elevator: Elevator.Id, floor: Floor.Id)(implicit timeout: Timeout): Future[Boolean] = {
		actor ? (MoveElevator(elevator,floor,_))
	}

	def requestElevatorAt(elevator: Elevator.Id, floor: Floor.Id)(implicit timeout: Timeout): Future[Boolean] = {
		actor ? (RequestElevator(elevator,floor,_))
	}

	def tick()(implicit timeout: Timeout): Future[Unit] = {
		actor ? Tick
	}
}

object Building {
	sealed trait Message
	final case class GetLocation(elevator: Elevator.Id, replyTo: ActorRef[Floor.Id]) extends Message
	final case class GetVelocity(elevator: Elevator.Id, replyTo: ActorRef[Velocity]) extends Message
	final case class MoveElevator(elevator: Elevator.Id, floor: Floor.Id, replyTo: ActorRef[Boolean]) extends Message
	final case class RequestElevator(elevator: Elevator.Id, floor: Floor.Id, replyTo: ActorRef[Boolean]) extends Message
	final case class Tick(replyTo: ActorRef[Unit]) extends Message

	private def initialize(numElevators: Int, numFloors: Int)(implicit system: ActorSystem[Nothing], timeout: Timeout): Behavior[Message] = {
		val elevators = MMap.empty[Elevator.Id,Elevator]
		val floors = Await.result(Floors.create(numFloors),Duration.Inf)
		(0 to numElevators).foreach(i => {
			val id = Elevator.Id(i)
			val elevator: Elevator = Await.result(Elevator.create(id),Duration.Inf)
			elevators.put(id,elevator)
			floors.setLocation(id,Floor.Id(0))
		})
		listening(elevators.toMap,floors)
	}

	private def listening(elevators: Map[Elevator.Id,Elevator], floors: Floors)(implicit system: ActorSystem[Nothing]): Behavior[Message] = {
		implicit val ec: ExecutionContext = system.executionContext
		implicit val t: Timeout = 3 seconds

		Actor.immutablePartial {
			case (_, GetLocation(id,replyTo)) =>
				floors.getLocation(id).foreach(fid => replyTo ! fid)
				Actor.same

			case (_, GetVelocity(id,replyTo)) =>
				elevators(id).velocity().foreach(vel => replyTo ! vel)
				Actor.same

			case (_, MoveElevator(elevator,floor,replyTo)) =>
				val moved = Await.result(floors.moveElevator(elevator,floor), Duration.Inf)
				replyTo ! moved
				val ref = elevators(elevator)
				ref.loseVelocity()
				ref.removeDestination(floor)
				listening(elevators, floors)

			case (_, RequestElevator(elevator, floor, replyTo)) =>
				if (floors.isLegalFloor(floor)) {
					replyTo ! true
					val vel = Await.result(floors.getRelativeVelocity(elevator,floor), Duration.Inf)
					val ref = elevators(elevator)
					ref.addDestination(floor)
					ref.setVelocity(vel)
					listening(elevators, floors)
				} else {
					replyTo ! false
					Actor.same
				}

			case (_, Tick(replyTo)) =>
				Future.sequence(elevators.map {
					case (id, el) =>
						// apply velocity change, and get new velocity as response
						el.velocity().flatMap(vel => {
							floors.updateLocation(elevators(id), vel).map(v => {
								el.setVelocity(v)
							})
						})
				}).foreach($ => replyTo ! ())
				Actor.same
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
