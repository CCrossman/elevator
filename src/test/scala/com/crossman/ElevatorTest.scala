package com.crossman

import akka.typed.ActorSystem
import akka.typed.scaladsl.Actor
import akka.util.Timeout
import com.crossman.Elevator.Velocity
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
			f <- building.getLocation(Elevator.Id(1))
		} yield {
			f shouldEqual Floor.Id(0)
		}
	}

	test("We can move an elevator to another floor") {
		for {
			building <- Building.create(5,6)
			f$1 <- building.getLocation(Elevator.Id(0))
			b$1 <- building.moveElevatorToFloor(Elevator.Id(0),Floor.Id(5))
			f$2 <- building.getLocation(Elevator.Id(0))
			b$2 <- building.moveElevatorToFloor(Elevator.Id(0),Floor.Id(5))
			f$3 <- building.getLocation(Elevator.Id(0))
		} yield {
			b$1 shouldBe true
			b$2 shouldBe false
			f$1 shouldEqual Floor.Id(0)
			f$2 shouldEqual Floor.Id(5)
			f$3 shouldEqual Floor.Id(5)
		}
	}

	test("An elevator cannot move to an illegal floor") {
		for {
			building <- Building.create(5,1)
			f$1 <- building.getLocation(Elevator.Id(0))
			b$1 <- building.moveElevatorToFloor(Elevator.Id(0),Floor.Id(2))
			f$2 <- building.getLocation(Elevator.Id(0))
			b$2 <- building.moveElevatorToFloor(Elevator.Id(0),Floor.Id(-1))
			f$3 <- building.getLocation(Elevator.Id(0))
		} yield {
			b$1 shouldBe false
			b$2 shouldBe false
			f$1 shouldEqual Floor.Id(0)
			f$2 shouldEqual Floor.Id(0)
			f$3 shouldEqual Floor.Id(0)
		}
	}

	test("We can request an elevator") {
		for {
			building <- Building.create(1, numFloors = 5)
			f$1 <- building.getLocation(Elevator.Id(0))
			b$1 <- building.requestElevatorAt(Elevator.Id(0),Floor.Id(4))
			v$1 <- building.getVelocity(Elevator.Id(0))
		} yield {
			b$1 shouldEqual true
			f$1 shouldEqual Floor.Id(0)
			v$1 shouldEqual Velocity.Up
		}
	}

	test("We can request an elevator after it started moving") {
		for {
			building <- Building.create(1, numFloors = 5)
			f$1 <- building.getLocation(Elevator.Id(0))
			b$1 <- building.requestElevatorAt(Elevator.Id(0),Floor.Id(4))
			_   <- building.tick()
			f$2 <- building.getLocation(Elevator.Id(0))
			_   <- building.tick()
			f$3 <- building.getLocation(Elevator.Id(0))
			b$2 <- building.requestElevatorAt(Elevator.Id(0),Floor.Id(3))
			_   <- building.tick()
			f$4 <- building.getLocation(Elevator.Id(0))
			v$1 <- building.getVelocity(Elevator.Id(0))
			_   <- building.tick()
			v$2 <- building.getVelocity(Elevator.Id(0))
			f$5 <- building.getLocation(Elevator.Id(0))
			_   <- building.tick()
			v$3 <- building.getVelocity(Elevator.Id(0))
			f$6 <- building.getLocation(Elevator.Id(0))
			_   <- building.tick()
			f$7 <- building.getLocation(Elevator.Id(0))
			_   <- building.tick()
			f$8 <- building.getLocation(Elevator.Id(0))
			_   <- building.tick()
			f$9 <- building.getLocation(Elevator.Id(0))
			_   <- building.tick()
		} yield {
			b$1 shouldEqual true
			b$2 shouldEqual true

			f$1 shouldEqual Floor.Id(0)
			f$2 shouldEqual Floor.Id(1)
			f$3 shouldEqual Floor.Id(2)
			f$4 shouldEqual Floor.Id(3)
			v$1 shouldEqual Velocity.Countdown(2)
			v$2 shouldEqual Velocity.Countdown(1)
			f$5 shouldEqual Floor.Id(3)
			v$3 shouldEqual Velocity.Up
			f$6 shouldEqual Floor.Id(3)
			f$7 shouldEqual Floor.Id(4)
			f$8 shouldEqual Floor.Id(4)
			f$9 shouldEqual Floor.Id(4)
		}
	}

	test("Find two destinations eventually") {
		for {
			building <- Building.create(1, numFloors = 3)
			_   <- building.requestElevatorAt(Elevator.Id(0),Floor.Id(2))
			_   <- building.tick()
			f$1 <- building.getLocation(Elevator.Id(0))
			_   <- building.requestElevatorAt(Elevator.Id(0),Floor.Id(0))
			_   <- building.tick()
			f$2 <- building.getLocation(Elevator.Id(0))
			_   <- building.tick()
			v$1 <- building.getVelocity(Elevator.Id(0))
			_   <- building.tick()
			f$3 <- building.getLocation(Elevator.Id(0))
			v$2 <- building.getVelocity(Elevator.Id(0))
			_   <- building.tick()
			f$4 <- building.getLocation(Elevator.Id(0))
			v$3 <- building.getVelocity(Elevator.Id(0))
			_   <- building.tick()
			f$5 <- building.getLocation(Elevator.Id(0))
			v$4 <- building.getVelocity(Elevator.Id(0))
			_   <- building.tick()
			f$6 <- building.getLocation(Elevator.Id(0))
			v$5 <- building.getVelocity(Elevator.Id(0))
			_   <- building.tick()
			f$7 <- building.getLocation(Elevator.Id(0))
			v$6 <- building.getVelocity(Elevator.Id(0))

		} yield {
			f$1 shouldEqual Floor.Id(1)
			f$2 shouldEqual Floor.Id(0)
			v$1 shouldEqual Velocity.Countdown(1)
			f$3 shouldEqual Floor.Id(0)
			v$2 shouldEqual Velocity.Up
			f$4 shouldEqual Floor.Id(1)
			v$3 shouldEqual Velocity.Up
			f$5 shouldEqual Floor.Id(2)
			v$4 shouldEqual Velocity.Countdown(2)
			f$6 shouldEqual Floor.Id(2)
			v$5 shouldEqual Velocity.Countdown(1)
			f$7 shouldEqual Floor.Id(2)
			v$6 shouldEqual Velocity.Still
		}
	}

	test("Find three destinations eventually") {
		for {
			building <- Building.create(1, numFloors = 3)
			_   <- building.requestElevatorAt(Elevator.Id(0),Floor.Id(1))
			_   <- building.requestElevatorAt(Elevator.Id(0),Floor.Id(0))
			_   <- building.requestElevatorAt(Elevator.Id(0),Floor.Id(2))
			_   <- building.tick()
			_   <- building.tick()
			_   <- building.tick()
			_   <- building.tick()
			_   <- building.tick()
			_   <- building.tick()
			_   <- building.tick()
			_   <- building.tick()
			_   <- building.tick()
			_   <- building.tick()
			v$1 <- building.getVelocity(Elevator.Id(0))
		} yield {
			v$1 shouldEqual Velocity.Still
		}
	}
}
