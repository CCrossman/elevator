package com.crossman

import akka.actor.Scheduler
import akka.typed.scaladsl.Actor
import akka.typed.{ActorRef, ActorSystem, Behavior}
import akka.util.Timeout
import com.crossman.Elevator._

import scala.concurrent.{ExecutionContext, Future}

case class Elevator(id: Elevator.Id, actor: ActorRef[Elevator.Message])(implicit system: ActorSystem[Nothing])  {
	import akka.typed.scaladsl.AskPattern._
	implicit val ec: ExecutionContext = system.executionContext
	implicit val scheduler: Scheduler = system.scheduler

	def addDestination(floor: Floor.Id): Unit = {
		actor ! AddDestination(floor)
	}

	def destinations()(implicit timeout: Timeout): Future[Set[Floor.Id]] = {
		actor ? GetDestinations
	}

	def velocity()(implicit timeout: Timeout): Future[Velocity] = {
		actor ? GetVelocity
	}

	def loseVelocity(): Unit = {
		setVelocity(Velocity.Still)
	}

	def removeDestination(floor: Floor.Id): Unit = {
		actor ! RemoveDestination(floor)
	}

	def setVelocity(delta: Int): Unit = {
		if (delta > 0) {
			setVelocity(Velocity.Up)
		} else if (delta < 0) {
			setVelocity(Velocity.Down)
		} else {
			setVelocity(Velocity.Still)
		}
	}

	def setVelocity(velocity: Velocity): Unit = {
		actor ! SetVelocity(velocity)
	}
}

object Elevator {
	final case class Id(value: Int) extends AnyVal

	sealed trait Message
	final case class AddDestination(floor: Floor.Id) extends Message
	final case class GetDestinations(replyTo: ActorRef[Set[Floor.Id]]) extends Message
	final case class GetVelocity(replyTo: ActorRef[Velocity]) extends Message
	final case class RemoveDestination(floor: Floor.Id) extends Message
	final case class SetVelocity(velocity: Velocity) extends Message

	private def initialize(destinations: Set[Floor.Id], velocity: Velocity): Behavior[Message] = Actor.immutable {
		case (_, AddDestination(floor)) =>
			initialize(destinations + floor, velocity)

		case (_, GetDestinations(replyTo)) =>
			replyTo ! destinations
			Actor.same

		case (_, GetVelocity(replyTo)) =>
			replyTo ! velocity
			Actor.same

		case (_, RemoveDestination(floor)) =>
			initialize(destinations - floor, velocity)

		case (_, SetVelocity(vel)) =>
			initialize(destinations,vel)
	}

	def create(id: Id)(implicit system: ActorSystem[Nothing], timeout: Timeout): Future[Elevator] = {
		implicit val ec: ExecutionContext = system.executionContext
		val fut = system.systemActorOf[Message](initialize(Set.empty,Velocity.Still), "elevator$" + id.value)
		fut.map { actor =>
			Elevator(id,actor)
		}
	}

	sealed trait Velocity
	object Velocity {
		final case class Countdown(value: Int) extends Velocity
		final case object Up extends Velocity
		final case object Down extends Velocity
		final case object Still extends Velocity
	}
}