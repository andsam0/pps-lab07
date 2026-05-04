package ex2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RobotSpec extends AnyFlatSpec with Matchers:
  "A SimpleRobot" should "turn correctly" in:
    val robot = new SimpleRobot((0, 0), Direction.North)

    robot.turn(Direction.East)
    robot.direction should be(Direction.East)

    robot.turn(Direction.South)
    robot.direction should be(Direction.South)

    robot.turn(Direction.West)
    robot.direction should be(Direction.West)

    robot.turn(Direction.North)
    robot.direction should be(Direction.North)

  it should "act correctly" in:
    val robot = new SimpleRobot((0, 0), Direction.North)

    robot.act()
    robot.position should be((0, 1))

    robot.turn(Direction.East)
    robot.act()
    robot.position should be((1, 1))

    robot.turn(Direction.South)
    robot.act()
    robot.position should be((1, 0))

    robot.turn(Direction.West)
    robot.act()
    robot.position should be((0, 0))

  "A RobotWithBattery" should "stop executing commands when battery is depleted" in :
    val robot = RobotWithBattery(SimpleRobot((0, 0), Direction.North), 2)

    robot.act()
    robot.act()
    //should ignore
    robot.act()
    robot.turn(Direction.East)

    robot.position should be((0, 2))
    robot.direction should be(Direction.North)

  "A RobotCanFail" should "always execute actions if failure probability is 0%" in :
    val robot = RobotCanFail(SimpleRobot((0, 0), Direction.North), 0)
    robot.act()
    robot.position should be((0, 1))

  it should "never execute actions if failure probability is 100%" in :
    val robot = RobotCanFail(SimpleRobot((0, 0), Direction.North), 100)
    robot.act()
    robot.turn(Direction.East)

    // Should remain in the initial state
    robot.position should be((0, 0))
    robot.direction should be(Direction.North)

  "A RobotRepeated" should "execute a single act() call multiple times" in :
    val robot = RobotRepeated(SimpleRobot((0, 0), Direction.North), 3)

    robot.act()

    // 1 call * 3 repetitions = 3 steps North
    robot.position should be((0, 3))
