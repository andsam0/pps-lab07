package ex2

import scala.util.Random

type Position = (Int, Int)
enum Direction:
  case North, East, South, West
  def turnRight: Direction = this match
    case Direction.North => Direction.East
    case Direction.East => Direction.South
    case Direction.South => Direction.West
    case Direction.West => Direction.North

  def turnLeft: Direction = this match
    case Direction.North => Direction.West
    case Direction.West => Direction.South
    case Direction.South => Direction.East
    case Direction.East => Direction.North

trait Robot:
  def position: Position
  def direction: Direction
  def turn(dir: Direction): Unit
  def act(): Unit

class SimpleRobot(var position: Position, var direction: Direction) extends Robot:
  def turn(dir: Direction): Unit = direction = dir
  def act(): Unit = position = direction match
    case Direction.North => (position._1, position._2 + 1)
    case Direction.East => (position._1 + 1, position._2)
    case Direction.South => (position._1, position._2 - 1)
    case Direction.West => (position._1 - 1, position._2)

  override def toString: String = s"robot at $position facing $direction"

class DumbRobot(val robot: Robot) extends Robot:
  export robot.{position, direction, act}
  override def turn(dir: Direction): Unit = {}
  override def toString: String = s"${robot.toString} (Dump)"

class LoggingRobot(val robot: Robot) extends Robot:
  export robot.{position, direction, turn}
  override def act(): Unit =
    robot.act()
    println(robot.toString)

class RobotWithBattery(val robot: Robot, var batteryLevel: Int) extends Robot:
  export robot.{position, direction}
  override def turn(dir: Direction): Unit =
    if batteryLevel > 0 then {
      robot.turn(dir)
      batteryLevel = batteryLevel - 1
    }
  override def act(): Unit =
    if batteryLevel > 0 then {
      robot.act()
      batteryLevel = batteryLevel - 1
    }
  override def toString: String = s"robot at ${robot.position} facing ${robot.direction} with remaining $batteryLevel battery"

class RobotCanFail(val robot: Robot, val failureProbability: Int) extends Robot:
  export robot.{position, direction}
  override def turn(dir: Direction): Unit =
    val failed: Boolean = Random.nextInt(101) < failureProbability
    if !failed then robot.turn(dir) else println("failed")

  override def act(): Unit =
    val failed: Boolean = Random.nextInt(101) < failureProbability
    if !failed then robot.act() else println("failed")

  override def toString: String = s"robot at ${robot.position} facing ${robot.direction} with remaining $failureProbability fail chance"

@main def testRobot(): Unit =
  val robot = LoggingRobot(SimpleRobot((0, 0), Direction.North))
  robot.act() // robot at (0, 1) facing North
  robot.turn(robot.direction.turnRight) // robot at (0, 1) facing East
  robot.act() // robot at (1, 1) facing East
  robot.act() // robot at (2, 1) facing East

  val robotBattery = LoggingRobot(RobotWithBattery(SimpleRobot((0, 0), Direction.North), 10))
  robotBattery.act() // robot at (0, 1) facing North with 4 battery
  robotBattery.turn(robotBattery.direction.turnRight) // robot at (0, 1) facing East with 3 battery
  robotBattery.act() // robot at (1, 1) facing East with 2 battery
  robotBattery.act() // robot at (2, 1) facing East with 1 battery

  val robotCanFail = LoggingRobot(RobotCanFail(SimpleRobot((0, 0), Direction.North), 50))
  robotCanFail.act() // robot at (0, 1) facing North
  robotCanFail.turn(robotCanFail.direction.turnRight) // robot at (0, 1) facing East
  robotCanFail.act() // robot at (1, 1) facing East
  robotCanFail.act() // robot at (2, 1) facing East
