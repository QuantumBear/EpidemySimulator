package simulator

import scala.math.random
import scala.collection.mutable.ListBuffer
import scala.language.postfixOps
import scala.annotation.tailrec

class EpidemySimulator extends Simulator {

  var running = false

  def randomBelow(i: Int) = (random * i).intValue()

  protected object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8

    val prevalenceRate = 0.01
    val moveWaitDays = 7
    val transmissibilityRate = 0.6
    val deadRate = 0.1

    val useReduceMobilityAct = true
  }

  import SimConfig._

  lazy val persons: List[Person] = {
    val persons = (1 to population) map (new Person(_)) toList

    // select the initial infected person
    val numInfected = (population * prevalenceRate).intValue;
    val infectedPersonIndex = (0 until numInfected).to[ListBuffer]

    infectedPersonIndex foreach { persons(_).infected = true }

    (numInfected until population) foreach {
      i =>
      {
        val m = randomBelow(i)
        if (m < numInfected) {
          persons(i).infected = true
          persons(infectedPersonIndex(m)).infected = false
          infectedPersonIndex.update(m, i)
        }
      }
    }
    persons
  }

  def getPersonAtRoom(x: Int, y: Int): List[Person] = {
    persons filter {
      p => p.row == x && p.col == y
    }
  }

  def isAnyStatePersonAtRoom(x: Int, y: Int, predict: Person => Boolean): Boolean = {
    getPersonAtRoom(x, y).count(predict(_)) > 0
  }

  class Person(val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    private var actions: List[Simulator#Action] = List()

    override def toString = {
      "id " + id + " infected " + infected + " sick " + sick + " immune " + immune + " dead " + dead + " row " + row + " col " + col
    }

    private def updateState() {
      // maybe get infected if people is healthy
      if (infected == false && currentTime > 0) {
        val anyInfectedPersonHere = isAnyStatePersonAtRoom(row, col, _.infected == true)
        val infectedPossibility = randomBelow(100) / 100.0
        if (infectedPossibility < transmissibilityRate) { // get infected
          infected = true
          sick = false
          immune = false
          dead = false
        }
      }
      // already got infected, translate in state transition
      if (infected == true && sick == false && immune == false && dead == false) {
        afterDelay(9) { // 6 days later, become sick
          sick = true
          infected = true
          immune = false
          dead = false
          updateState()
        }
        return
      }
      if (infected == true && sick == true && dead == false) {
        afterDelay(8) { // 8 days later, 25% possibility dead
          if ((randomBelow(100) / 100.0) < deadRate) { // dead, end state
            dead = true
            sick = true
            infected = true
            immune = false
          } else {
            afterDelay(2) { // if not dead, after 2 days to immune state
              immune = true
              sick = false
              infected = true
              dead = false
              updateState()
            }
          }
        }
        return
      }
      if (immune == true) {
        afterDelay(7) { // 7 days later become healthy, end state
          immune = false
          infected = false
          sick = false
          dead = false
          updateState()
        }
      }
      if (dead == true) {
        return
      }
    }

    private def generateNextPos(): (Int, Int) = {
      val nextPosArray = List(
        ((row + roomRows - 1) % roomRows, col),
        ((row + 1) % roomRows, col),
        (row, ((col + roomColumns - 1) % roomColumns)),
        (row, ((col + 1) % roomColumns)))
      val notGoPos = nextPosArray.filter(p => isAnyStatePersonAtRoom(p._1, p._2, i => i.sick == true))
      val possiblePos = nextPosArray.diff(notGoPos)
      if (possiblePos.length == 0) {
        (row, col)
      } else {
        val selectIndex = randomBelow(possiblePos.length)
        possiblePos(selectIndex)
      }
    }

    def move() {
      // people only move if not dead
      if (dead == true)
        return

      if (currentTime == 0 || infected == false) { // if not start transit state, into state transition
        updateState()
      }

      var nextMoveDay = randomBelow(moveWaitDays) + 1
      if (useReduceMobilityAct == true && sick == true) {
        nextMoveDay *= 2
      }
      afterDelay(nextMoveDay) {
        if (dead == false) {
          val nextPos = generateNextPos()
          row = nextPos._1; col = nextPos._2
        }
        move()
      }
    }
  }

  {
    persons foreach (_.move())
  }
}
