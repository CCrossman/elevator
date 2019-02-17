package com.crossman

import java.util.UUID

import akka.actor.Scheduler
import akka.typed.scaladsl.Actor
import akka.typed.{ActorRef, ActorSystem, Behavior}
import akka.util.Timeout
import com.crossman.Elevator.Velocity
import com.crossman.Floors.{GetLocation, SetLocation}

import scala.concurrent.{ExecutionContext, Future}

final case class Floors(numFloors: Int, actor: ActorRef[Floors.Message])(implicit system: ActorSystem[Nothing]) {
	import akka.typed.scaladsl.AskPattern._
	implicit val scheduler: Scheduler = system.scheduler
	implicit val ec: ExecutionContext = system.executionContext

	def getLocation(elevator: Elevator.Id)(implicit timeout: Timeout): Future[Floor.Id] = {
		actor ? (GetLocation(elevator,_))
	}

	def getRelativeVelocity(elevator: Elevator.Id, floor: Floor.Id)(implicit timeout: Timeout): Future[Velocity] = {
		getLocation(elevator).map(loc => {
			if (loc.value < floor.value) {
				Velocity.Up
			} else if (loc.value > floor.value) {
				Velocity.Down
			} else {
				Velocity.Still
			}
		})
	}

	def isLegalFloor(floor: Floor.Id): Boolean = {
		floor.value >= 0 && floor.value < numFloors
	}

	def moveElevator(elevator: Elevator.Id, floor: Floor.Id)(implicit timeout: Timeout): Future[Boolean] = {
		if (!isLegalFloor(floor)) {
			Future.successful(false)
		} else {
			getLocation(elevator).map(fid => {
				if (fid != floor) {
					setLocation(elevator, floor)
					true
				} else {
					false
				}
			})
		}
	}

	def setLocation(elevator: Elevator.Id, floor: Floor.Id): Unit = {
		actor ! SetLocation(elevator,floor)
	}

	def updateLocation(elevator: Elevator, velocity: Velocity)(implicit timeout: Timeout): Future[Velocity] = {
		getLocation(elevator.id).flatMap(fid => {
			elevator.destinations().map(dsts => {
				val newVelocity = velocity match {
					case Velocity.Up =>
						val fid$1 = Floor.Id(fid.value + 1)
						setLocation(elevator.id,fid$1)
						if (dsts.contains(fid$1)) {
							elevator.removeDestination(fid$1)
							Velocity.Countdown(2)
						} else {
							Velocity.Up
						}
					case Velocity.Down =>
						val fid$2 = Floor.Id(fid.value - 1)
						setLocation(elevator.id,fid$2)
						if (dsts.contains(fid$2)) {
							elevator.removeDestination(fid$2)
							Velocity.Countdown(2)
						} else {
							Velocity.Down
						}
					case Velocity.Countdown(n) if n > 1 =>
						Velocity.Countdown(n - 1)
					case Velocity.Countdown(1) =>
						// FIXME: go towards closest destination
						if (dsts.isEmpty) {
							Velocity.Still
						} else if (dsts.exists(f => f.value < fid.value)) {
							Velocity.Down
						} else {
							Velocity.Up
						}
					case v =>
						v
				}
				newVelocity
			})
		})
	}
}

object Floor {
	final case class Id(value: Int) extends AnyVal
}

object Floors {
	sealed trait Message
	final case class GetLocation(elevator: Elevator.Id, replyTo: ActorRef[Floor.Id]) extends Message
	final case class SetLocation(elevator: Elevator.Id, floor: Floor.Id) extends Message

	private def initialize(visitors: Map[Elevator.Id,Floor.Id]): Behavior[Message] = {
		Actor.immutablePartial {
			case (_, GetLocation(elevator,replyTo)) =>
				replyTo ! visitors(elevator)
				Actor.same

			case (_, SetLocation(elevator,floor)) =>
				initialize(visitors + ((elevator,floor)))
		}
	}

	def create(numFloors: Int)(implicit system: ActorSystem[Nothing], timeout: Timeout): Future[Floors] = {
		implicit val ec: ExecutionContext = system.executionContext
		val id: UUID = UUID.randomUUID()
		val fut = system.systemActorOf[Message](initialize(Map.empty), "floors$" + id)
		fut map { actor =>
			Floors(numFloors,actor)
		}
	}
}