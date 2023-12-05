package pp202302.assign4.cellular

case class CellState(name: String, index: Int)
case class Grid[A](cells: Vector[Vector[A]])

given vectorIL[State]: IndexedLike[Vector, Int, State] with
  def fill(size: Int)(elem: State): Vector[State] =
    Vector.fill(size)(elem)

  extension (l: Vector[State])
    def size: Int = l.length
    def get(i: Int): State = l(i)

given gridIL[State]: IndexedLike[Grid, (Int, Int), State] with
  def fill(size: (Int, Int))(elem: State): Grid[State] =
    val cells = Vector.fill(size._1)(Vector.fill(size._2)(elem))
    Grid(cells)

  extension (l: Grid[State])
    def size: (Int, Int) = (l.cells.length, l.cells(0).length)
    def get(i: (Int, Int)): State = l.cells(i._1)(i._2)

given vectorAuto[State](using
    cr: CellRule[Vector, Int, State],
    il: IndexedLike[Vector, Int, State]
): CellAutomata[Vector, Int, State] with
  def empty(size: Int): Vector[State] =
    Vector.fill(size)(cr.defaultState)

  extension (ca: Vector[State])
    def setStateAt(pos: Int, state: State): Vector[State] =
      ca.updated(pos, state)
    def neighborsAt(pos: Int): Vector[State] = {
      if pos == 0 then Vector(cr.defaultState, ca(pos), ca(pos + 1))
      else if pos == ca.length - 1 then
        Vector(ca(pos - 1), ca(pos), cr.defaultState)
      else Vector(ca(pos - 1), ca(pos), ca(pos + 1))
    }
    def step: Vector[State] = {
      ca.indices.map(x => cr.nextState(ca(x), neighborsAt(x))).toVector
    }

given gridAuto[State](using
    cr: CellRule[Grid, (Int, Int), State],
    il: IndexedLike[Grid, (Int, Int), State]
): CellAutomata[Grid, (Int, Int), State] with
  def empty(size: (Int, Int)): Grid[State] = {
    gridIL.fill(size)(cr.defaultState)
  }

  // TODO: UNCOMMENT BELOW AND FILL IT!
  extension (ca: Grid[State])
    def setStateAt(pos: (Int, Int), state: State): Grid[State] = {
      val newCell =
        ca.cells.updated(pos._1, ca.cells(pos._1).updated(pos._2, state))
      Grid(newCell)
    }

    def neighborsAt(pos: (Int, Int)): Grid[State] = {
      val x = pos._1
      val y = pos._2

      val neighborCells: Vector[Vector[State]] = Vector[Vector[State]](
      Vector[State](getCellSafe(x - 1, y - 1), getCellSafe(x - 1, y), getCellSafe(x - 1, y + 1)),
      Vector[State](getCellSafe(x, y - 1),  getCellSafe(x, y + 1)),
      Vector[State](getCellSafe(x + 1, y - 1), getCellSafe(x + 1, y), getCellSafe(x + 1, y + 1))
      )

      Grid(neighborCells)
    }
    def getCellSafe(pos: (Int, Int)): State = {
      val (x, y) = pos
      if (x >= 0 && x < ca.size._1 && y >= 0 && y < ca.size._2) {
        (ca.cells(x)(y))
      } 
      else {
        cr.defaultState
      }
    }
    def step: Grid[State] = {
      //Grid(ca.cells.map(row => row.step).toVector) why not?
      Grid(ca.cells.zipWithIndex.map { 
        case (row, rowIndex) => row.indices.map { colIndex => cr.nextState(row(colIndex), ca.neighborsAt((rowIndex, colIndex)))}.toVector
  }.toVector)
    }
