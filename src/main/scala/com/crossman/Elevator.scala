package com.crossman

import akka.typed.scaladsl.Actor
import akka.typed.{ActorRef, ActorSystem, Behavior}
import akka.util.Timeout

import scala.concurrent.{ExecutionContext, Future}

case class Elevator(id: Elevator.Id, actor: ActorRef[Elevator.Message]) {

}

object Elevator {
	final case class Id(value: Int) extends AnyVal

	sealed trait Message

	private def listening(): Behavior[Message] = {
		Actor.ignore
	}

	def create(id: Id)(implicit system: ActorSystem[Nothing], timeout: Timeout): Future[Elevator] = {
		implicit val ec: ExecutionContext = system.executionContext
		val fut = system.systemActorOf[Message](listening(), s"elevator${id.value}")
		fut map { actor =>
			Elevator(id,actor)
		}
	}
}