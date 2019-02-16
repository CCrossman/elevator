package com.crossman

import akka.typed.ActorSystem
import akka.typed.scaladsl.Actor
import akka.util.Timeout
import org.scalatest.{AsyncFunSuite, FunSuite, Matchers}

import scala.concurrent.duration._
import scala.language.postfixOps

class ElevatorTest extends AsyncFunSuite with Matchers {
	implicit val system: ActorSystem[Nothing] = ActorSystem.create(Actor.empty,"test")
	implicit val timeout: Timeout = 3 seconds

	test("We can make one or more elevators for a building") {
		Building.create(5,6) map { building =>
			building.numElevators shouldEqual 5
			building.numFloors shouldEqual 6
		}
	}

	test("We can ask where an elevator is") {
		for {
			building <- Building.create(5,6)
			f <- building.getFloor(Elevator.Id(1))
		} yield {
			f shouldEqual Floor.Id(0)
		}
	}

	test("We can move an elevator to another floor") {
		for {
			building <- Building.create(5,6)
			f$1 <- building.getFloor(Elevator.Id(0))
			b$1 <- building.moveElevatorToFloor(Elevator.Id(0),Floor.Id(5))
			f$2 <- building.getFloor(Elevator.Id(0))
			b$2 <- building.moveElevatorToFloor(Elevator.Id(0),Floor.Id(5))
		} yield {
			b$1 shouldBe true
			b$2 shouldBe false
			f$1 shouldEqual Floor.Id(0)
			f$2 shouldEqual Floor.Id(5)
		}
	}
}
