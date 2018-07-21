package demo

import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {

    // Set the max possible size of the grid
    // (-customGridBound, -customGridBound) to (customGridBound, customGridBound)
    val customGridBound = 49

    def go(cells: CellGrid = Vector.empty[Vector[Cell]], history: List[CellGrid] = List.empty[CellGrid]): Unit = {
      val grid = new Grid(cells, customGridBound)

      println(s"Tick: ${history.size}")
      println(grid.toString)
      println("="*grid.gridCells.size*2)

      val updatedCells = grid.updateCells()
      if (updatedCells.isEmpty) {
        if (grid.livingCellsInGrid)
          println("Population is stable!")
        else
          println("Extinction...")
      }
      else if (history.contains(updatedCells))
        println("We've seen this formation before! Population will survive!")
      else
        go(updatedCells, history :+ grid.gridCells)
    }

    go()
  }



  type CellGrid = Vector[Vector[Cell]]



  case class Cell(x: Int, y: Int, isAlive: Boolean) {
    override def toString: String = if (isAlive) "*" else "x"
  }



  class Grid(cells: CellGrid, upperBound: Int = 49) {

    // Generate a random bound for the grid between 1 and 50 by default
    // Min (1) example:  -1 to 1 for both xRange and yRange.
    // Max (50) example: -50 to 50 for both xRange and yRange
    // Set minimumBound to ensure we don't start with empty grid
    def generateInitialCells: CellGrid = {
      val minimumBound = 1
      val randomBound = new Random().nextInt(upperBound) + minimumBound
      val range = Vector.range(-randomBound, randomBound+1)

      // Iterate over the cells in the grid and assign a random lifecycle state
      range.map(x => range.map(y => Cell(x, y, math.random > .5)))
    }

    // Test if the given coordinates are valid (within the bounds of the grid)
    private def gridIndexValid(x: Int, y: Int): Boolean = gridCells.indices.contains(x) && gridCells.indices.contains(y)

    // Determines if the cell is alive or dead for the next tick
    private def updateCell(centerXCoord: Int, centerYCoord: Int): Boolean = {
      val range = -1 to 1
      // Check the 3x3 around (and including) the center point and total the living cells
      val totalAliveInSquare = range.foldLeft(0) { (acc, x) =>
        acc + range.foldLeft(0) { (acc, y) =>
          // assume any coordinates outside of the grid are dead
          if (gridIndexValid(x+centerXCoord, y+centerYCoord)) {
            val updateIfAlive: Int = if(gridCells(x+centerXCoord)(y+centerYCoord).isAlive) 1 else 0
            acc + updateIfAlive
          } else {
            acc
          }
        }
      }

      val centerAlive: Boolean = gridCells(centerXCoord)(centerYCoord).isAlive
      val totalLivingNeighbors = if (centerAlive) totalAliveInSquare - 1 else totalAliveInSquare

      (centerAlive, totalLivingNeighbors) match {
        case (true, tln)  if tln < 2  => false
        case (true, tln)  if tln > 3  => false
        case (false, tln) if tln == 3 => true
        case (_ , _)                  => centerAlive
      }
    }

    def updateCells(): CellGrid = {
      val range = Vector.range(0, gridBound)
      val updatedCells = range.map(x => range.map(y => gridCells(x)(y).copy(isAlive = updateCell(x, y))))

      if (updatedCells == gridCells)
        Vector.empty[Vector[Cell]]
      else
        updatedCells
    }


    def livingCellsInGrid: Boolean = {
      val range = Vector.range(0, gridBound)
      range.foldLeft(false)((acc, x) => acc || range.foldLeft(false)((acc, y) => acc || gridCells(x)(y).isAlive))
    }

    // If no cells are supplied to the Grid constructor, a new grid of random cells will be created
    // 2D Vector with indexes translating to coordinates by the following:
    // Coordinate == index - randomBound
    // gridCells(0)(0) corresponds to cell at coordinates (-randomBound, -randomBound)
    // and gridCells(randomBound * 2)(randomBound * 2) corresponds to cell coordinates (randomBound, randomBound)
    val gridCells: CellGrid = if (cells.isEmpty) generateInitialCells else cells
    private val gridBound: Int = gridCells.size

    override def toString: String = {
      val xRange = 0 until gridBound
      val yRange = xRange.reverse

      def cellString (x: Int, y: Int): String = {
        val endChar = if (y == 0) "\n" else " "
        s"${gridCells(x)(y).toString}$endChar"
      }

      xRange.foldLeft("")((acc, x) => acc.concat(yRange.foldLeft("")((acc, y) => acc.concat(cellString(x, y)))))
    }
  }
}
